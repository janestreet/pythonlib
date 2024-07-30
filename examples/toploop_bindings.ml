(* This module provides bindings to the ocaml toplevel.
   This allows one to run ocaml blocks from python and exchange values between the
   ocaml and python runtimes.

   The toplevel module returns [Obj.t] object so we rely heavily on [Obj.repr]
   to get some typed values out of it. This should be type-safe as we also
   ask the toplevel loop to check the types.
*)
module F = Format
open Base
open Python_lib
open Python_lib.Let_syntax
module Typerep = Typerep_lib.Std.Typerep

let is_initialized = ref false

let maybe_initialize () =
  if not !is_initialized
  then (
    is_initialized := true;
    Clflags.debug := true;
    Clflags.verbose := false;
    ignore @@ Warnings.parse_options false "-58";
    Location.formatter_for_warnings := F.err_formatter;
    Toploop.set_paths ();
    !Toploop.toplevel_startup_hook ();
    (* required for side-effect initialization in Topdirs *)
    Toploop.initialize_toplevel_env ();
    let lexing = Lexing.from_string "type pyobject;;" in
    let phrases = !Toploop.parse_use_file lexing in
    List.iter phrases ~f:(fun phrase ->
      let ok = Toploop.execute_phrase false F.std_formatter phrase in
      ignore (ok : bool));
    Py_typerep.Named_types.register_exn
      ~name:"pyobject"
      ~ocaml_type:"pyobject"
      ~python_to_ocaml:Fn.id
      ~ocaml_to_python:Fn.id)
;;

let exn_to_string exn ~code =
  let print_loc _ _report ppf (location : Location.t) =
    F.fprintf
      ppf
      "ocaml evaluation error on lines %d:%d to %d:%d\n"
      location.loc_start.pos_lnum
      location.loc_start.pos_cnum
      location.loc_end.pos_lnum
      location.loc_end.pos_cnum
  in
  let default_printer = Location.default_report_printer () in
  let report report_printer report ppf x =
    let location = report.Location.main.loc in
    F.pp_print_newline ppf ();
    let min_line_number = location.loc_start.pos_lnum - 5 in
    let max_line_number = location.loc_end.pos_lnum + 5 in
    String.rstrip code ~drop:(function
      | ' ' | '\r' | '\n' | '\t' -> true
      | _ -> false)
    |> String.split ~on:'\n'
    |> List.filter_mapi ~f:(fun lnum line ->
         let lnum = 1 + lnum in
         if min_line_number <= lnum && lnum <= max_line_number
         then (
           let marker =
             if location.loc_start.pos_lnum <= lnum && lnum <= location.loc_end.pos_lnum
             then ">"
             else " "
           in
           Some (Printf.sprintf "%s%3d: %s" marker lnum line))
         else None)
    |> String.concat ~sep:"\n"
    |> F.pp_print_string ppf;
    F.pp_print_newline ppf ();
    default_printer.Location.pp_main_txt report_printer report ppf x
  in
  let buffer = Buffer.create 256 in
  let formatter = F.formatter_of_buffer buffer in
  let report_printer () : Location.report_printer =
    { default_printer with
      Location.pp_main_loc = print_loc
    ; pp_submsg_loc = print_loc
    ; pp_main_txt = report
    }
  in
  Location.report_printer := report_printer;
  Location.report_exception formatter exn;
  Buffer.contents buffer
;;

let toploop_eval str ~verbose =
  try
    maybe_initialize ();
    let lexing = Lexing.from_string str in
    let phrases = !Toploop.parse_use_file lexing in
    List.iter phrases ~f:(fun phrase ->
      let ok = Toploop.execute_phrase verbose F.std_formatter phrase in
      ignore (ok : bool));
    F.pp_print_flush F.std_formatter ()
  with
  | Py.Err _ as err -> raise err
  | exn -> raise (Py.Err (SyntaxError, exn_to_string exn ~code:str))
;;

let dummy_loc =
  { Warnings.loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = false }
;;

let toploop_eval_and_get typerep str =
  let eval_value (type a) (typerep : a Typerep.t) =
    toploop_eval
      ~verbose:false
      (Printf.sprintf "let out : %s = (%s);;" (Py_typerep.to_ocaml typerep) str);
    let path, _ = Env.lookup_value ~loc:dummy_loc (Lident "out") !Toploop.toplevel_env in
    let obj = Toploop.eval_value_path !Toploop.toplevel_env path in
    Py_typerep.ocaml_to_python typerep (Stdlib.Obj.obj obj)
  in
  let (T typerep) = Py_typerep.parse typerep in
  eval_value typerep
;;

let toploop_eval_and_get_no_type str =
  toploop_eval ~verbose:false (Printf.sprintf "let out = (%s);;" str);
  let path, value_description =
    Env.lookup_value ~loc:dummy_loc (Lident "out") !Toploop.toplevel_env
  in
  let obj = Toploop.eval_value_path !Toploop.toplevel_env path in
  let (T typerep) =
    Type.of_type_desc
      (Types.get_desc value_description.val_type)
      ~env:(Module_env.create ())
    |> Or_error.ok_exn
    |> Py_typerep.of_type
  in
  Py_typerep.ocaml_to_python typerep (Stdlib.Obj.obj obj)
;;

let register_module ~module_name =
  let modl = Py_module.create module_name in
  Py_module.set_unit
    modl
    "eval"
    (let%map_open str = positional_only "str" string ~docstring:"ocaml code to run"
     and verbose = keyword "verbose" bool ~default:false ~docstring:"verbose" in
     fun () -> toploop_eval str ~verbose)
    ~docstring:
      {|
    Evaluates an ocaml expression.

    This takes a argument a string containing some ocaml code. This string is parsed,
    typechecked and evaluated in a toplevel.
    The global variable scope is shared between multiple calls to this function.
      |};
  Py_module.set_function
    modl
    "get"
    (function
     | [| str |] -> Py.String.to_string str |> toploop_eval_and_get_no_type
     | [| typerep; str |] ->
       toploop_eval_and_get (Py.String.to_string typerep) (Py.String.to_string str)
     | _ -> raise (Py.Err (SyntaxError, "expected one or two arguments")))
    ~docstring:
      {|
    Evaluates an ocaml expression and returns the result as a python object.

    This takes one or two arguments. When two arguments are given, the first
    one is a type representation of the object to be transfered from ocaml to
    python. The second one is a string containing some ocaml code. This string
    is parsed, typechecked (its type has to match the type argument) and
    evaluated in a toplevel in the same way it is when running eval.

    Supported types can involve bool, int, string, float, list, option,
    only a single arrow is allowed in which case a function is returned.
    Examples of types would be:
        - int
        - unit -> string
        - (string * int option) list -> string * string

    Returns:
        A python object which type depends on the type argumnent.
    |};
  Py_module.set_unit
    modl
    "add_topdir"
    (let%map_open dir = positional_only "dir" string ~docstring:"directory to add" in
     fun () ->
       if !is_initialized then failwith "can only add directories before initialization.";
       Topdirs.dir_directory dir)
    ~docstring:
      {|
    Adds a new top-level directory in the cmi search path.

    This can only be executed before the first call to eval or dir. Otherwise an exception
    is raised.
    |};
  Py_module.set_unit
    modl
    "add_named_type"
    (let%map_open name = positional_only "name" string ~docstring:"type name"
     and ocaml_type = positional_only "ocaml_type" string ~docstring:"ocaml type" in
     fun () -> Py_typerep.register_named_type ~name ~ocaml_type)
    ~docstring:
      {|
    Registers an ocaml type so that it can be transfered to python in a caspule.
    |}
;;

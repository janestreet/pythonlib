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
module Topdirs = Opttopdirs
module Toploop = Opttoploop
module Typerep = Typerep_lib.Std.Typerep

let is_initialized = ref false

let maybe_initialize () =
  if not !is_initialized
  then (
    is_initialized := true;
    Clflags.debug := true;
    Clflags.verbose := false;
    Warnings.parse_options false "-58";
    Location.formatter_for_warnings := F.err_formatter;
    Toploop.set_paths ();
    !Toploop.toplevel_startup_hook ();
    (* required for side-effect initialization in Topdirs *)
    Toploop.initialize_toplevel_env ())
;;

let exn_to_string exn ~code =
  let print_loc _ _report ppf (location : Location.t) =
    F.fprintf
      ppf
      "ocaml evaluation error on lines %d:%d to %d:%d"
      location.loc_start.pos_lnum
      location.loc_start.pos_cnum
      location.loc_end.pos_lnum
      location.loc_end.pos_cnum
  in
  let report report_printer report ppf x =
    report_printer.Location.pp_main_txt report_printer report ppf x;
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
          if location.loc_start.pos_lnum <= lnum
          && lnum <= location.loc_end.pos_lnum
          then ">"
          else " "
        in
        Some (Printf.sprintf "%s%3d: %s" marker lnum line))
      else None)
    |> String.concat ~sep:"\n"
    |> F.pp_print_string ppf
  in
  let buffer = Buffer.create 256 in
  let formatter = F.formatter_of_buffer buffer in
  let report_printer () : Location.report_printer =
    let default_printer = Location.default_report_printer () in
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
  | exn -> raise (Py.Err (SyntaxError, exn_to_string exn ~code:str))
;;

let toploop_eval_and_get typerep str =
  let eval_value (type a) (typerep : a Typerep.t) =
    toploop_eval
      ~verbose:false
      (Printf.sprintf "let out : %s = (%s);;" (Py_typerep.to_ocaml typerep) str);
    let path, _ = Env.lookup_value (Lident "out") !Toploop.toplevel_env in
    let obj = Toploop.eval_value_path !Toploop.toplevel_env path in
    Py_typerep.ocaml_to_python typerep (Caml.Obj.obj obj)
  in
  let eval_and_get_fn (type a b) (lhs : a Typerep.t) (rhs : b Typerep.t) =
    toploop_eval
      ~verbose:false
      (Printf.sprintf
         "let out : %s -> %s = (%s);;"
         (Py_typerep.to_ocaml lhs)
         (Py_typerep.to_ocaml rhs)
         str);
    let path, _ = Env.lookup_value (Lident "out") !Toploop.toplevel_env in
    let fn : a -> b =
      Toploop.eval_value_path !Toploop.toplevel_env path |> Caml.Obj.obj
    in
    Py.Callable.of_function ~docstring:str (fun args ->
      let args =
        match args with
        | [||] -> Py.none
        | [| v |] -> v
        | otherwise -> Py.Tuple.of_array otherwise
      in
      try
        Py_typerep.python_to_ocaml lhs args |> fn |> Py_typerep.ocaml_to_python rhs
      with
      | exn -> raise (Py.Err (ValueError, Exn.to_string exn)))
  in
  match Py_typerep.parse_maybe_fn typerep with
  | `value (T typerep) -> eval_value typerep
  | `fn (T lhs, T rhs) -> eval_and_get_fn lhs rhs
;;

let register_module ~module_name =
  let modl = Py_module.create module_name in
  Py_module.set_unit
    modl
    "eval"
    [%map_open
      let str = positional "str" string ~docstring:"ocaml code to run"
      and verbose = keyword "verbose" bool ~default:false ~docstring:"verbose" in
      toploop_eval str ~verbose]
    ~docstring:
      {|
    Evaluates an ocaml expression.

    This takes a argument a string containing some ocaml code. This string is parsed,
    typechecked and evaluated in a toplevel.
    The global variable scope is shared between multiple calls to this function.
      |};
  Py_module.set
    modl
    "get"
    [%map_open
      let typerep = positional "typerep" string ~docstring:"typerep"
      and str = positional "str" string ~docstring:"ocaml code to run" in
      toploop_eval_and_get typerep str]
    ~docstring:
      {|
    Evaluates an ocaml expression and returns the result as a python object.

    This takes two arguments. The first one is a type representation of the
    object to be transfered from ocaml to python. The second one is a string
    containing some ocaml code. This string is parsed, typechecked (its
    type has to match the type argument) and evaluated in a toplevel in the
    same way it is when running eval.

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
    [%map_open
      let dir = positional "dir" string ~docstring:"directory to add" in
      if !is_initialized then failwith "can only add directories before initialization.";
      Topdirs.dir_directory dir]
    ~docstring:
      {|
    Adds a new top-level directory in the cmi search path.

    This can only be executed before the first call to eval or dir. Otherwise an exception
    is raised.
    |};
  Py_module.set_unit
    modl
    "add_named_type"
    [%map_open
      let name = positional "name" string ~docstring:"type name"
      and ocaml_type = positional "ocaml_type" string ~docstring:"ocaml type" in
      Py_typerep.register_named_type ~name ~ocaml_type]
    ~docstring:
      {|
    Registers an ocaml type so that it can be transfered to python in a caspule.
    |}
;;

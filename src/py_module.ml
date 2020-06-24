open Base
open Import

type t = pyobject

let create ?docstring module_name =
  let modl = Py.Import.add_module module_name in
  Option.iter docstring ~f:(Py.Module.set_docstring modl);
  modl
;;

let create_with_eval ~name ~py_source =
  let modl = Py.Import.add_module name in
  let modl_dict = Py.Module.get_dict modl in
  (* Set __file__ to "<pyml>", because it doesn't come from a real file that Python tools
     can expect to be able to read. It seems like non-filename values are acceptable in
     this case (see [1] which recommends "<string>")

     [1]: https://docs.python.org/3/library/functions.html#compile
  *)
  Py.Dict.set_item_string modl_dict "__file__" (python_of_string "<pyml>");
  Py.Dict.set_item_string modl_dict "__builtins__" (Py.Eval.get_builtins ());
  let _ = Py.Run.eval ~globals:modl_dict ~locals:modl_dict ~start:File py_source in
  modl
;;

let import = Py.import
let set_value = Py.Module.set
let pyobject t = t

let keywords_of_python pyobject =
  match Py.Type.get pyobject with
  | Null | None -> Ok (Map.empty (module String))
  | Dict ->
    (match Py.Dict.to_bindings_string pyobject |> Map.of_alist (module String) with
     | `Ok map -> Ok map
     | `Duplicate_key d -> Or_error.errorf "duplicate keyword %s" d)
  | otherwise ->
    Or_error.errorf "expected dict for keywords, got %s" (Py.Type.name otherwise)
;;

let wrap_ocaml_errors f =
  try f () with
  | Py.Err _ as pyerr -> raise pyerr
  | exn ->
    let backtrace = Backtrace.Exn.most_recent () in
    let msg =
      Printf.sprintf
        "ocaml error %s\n%s"
        (Exn.to_string_mach exn)
        (Backtrace.to_string backtrace)
    in
    raise (Py.Err (ValueError, msg))
;;

let set_function t ?docstring name fn =
  let fn args = wrap_ocaml_errors (fun () -> fn args) in
  Py.Module.set t name (Py.Callable.of_function ~name ?docstring fn)
;;

let set_function_with_keywords t ?docstring name fn =
  let fn args keywords =
    let keywords =
      match keywords_of_python keywords with
      | Ok keywords -> keywords
      | Error err -> raise (Py.Err (ValueError, Error.to_string_hum err))
    in
    wrap_ocaml_errors (fun () -> fn args keywords)
  in
  Py.Module.set t name (Py.Callable.of_function_with_keywords ~name ?docstring fn)
;;

let set t ?docstring name defunc =
  let docstring = Defunc.params_docstring ?docstring defunc in
  set_function_with_keywords t ~docstring name (Defunc.apply defunc)
;;

let set_unit t ?docstring name defunc =
  let docstring = Defunc.params_docstring ?docstring defunc in
  set_function_with_keywords t ~docstring name (fun args kwargs ->
    Defunc.apply defunc args kwargs;
    Py.none)
;;

let set_no_arg t ?docstring name fn =
  set_function_with_keywords t ?docstring name (fun args keywords ->
    if not (Array.is_empty args)
    then value_errorf "no positional argument expected (got %d)" (Array.length args);
    if not (Map.is_empty keywords)
    then value_errorf "no keyword argument expected (got %d)" (Map.length keywords);
    fn ())
;;

module Raw = struct
  let set = set
end

open Base
open Import

type t = pyobject

let create = Py.Import.add_module

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

let set_value = Py.Module.set

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
    let msg = Printf.sprintf "ocaml error %s" (Exn.to_string_mach exn) in
    raise (Py.Err (ValueError, msg))
;;

let set_function t ?docstring name fn =
  let fn args = wrap_ocaml_errors (fun () -> fn args) in
  Py.Module.set t name (Py.Callable.of_function ?docstring fn)
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
  Py.Module.set t name (Py.Callable.of_function_with_keywords ?docstring fn)
;;

let docstring_with_params ?docstring defunc =
  [ Defunc.params_docstring defunc; docstring ]
  |> List.filter_opt
  |> String.concat ~sep:"\n\n"
  |> Printf.sprintf "\n%s"
;;

let set t ?docstring name defunc =
  let docstring = docstring_with_params ?docstring defunc in
  set_function_with_keywords t ~docstring name (Defunc.apply defunc)
;;

let set_unit t ?docstring name defunc =
  let docstring = docstring_with_params ?docstring defunc in
  set_function_with_keywords t ~docstring name (fun args kwargs ->
    Defunc.apply defunc args kwargs;
    Py.none)
;;

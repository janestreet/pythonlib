open Base
open Import

type t =
  { module_name : string
  ; modl : pyobject
  }

let create ?docstring module_name =
  let modl = Py.Import.add_module module_name in
  Option.iter docstring ~f:(Py.Module.set_docstring modl);
  { module_name; modl }
;;

let module_name t = t.module_name

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
  { module_name = name; modl }
;;

let import module_name = { module_name; modl = Py.import module_name }
let set_value t = Py.Module.set t.modl
let pyobject t = t.modl

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

(* Maybe using a trie rather than iterating through a list of prefixes would be
   more efficient? *)
let excluded_prefixes =
  [ "Async_kernel__Deferred"
  ; "Async_kernel__Job_queue.run_job"
  ; "Async_kernel__Monitor.Exported_for_scheduler."
  ; "Base__Error.raise"
  ; "Base__Exn.protect"
  ; "Base__Exn.raise_with_original"
  ; "Base__Exn.reraise"
  ; "Base__Or_error.ok_exn"
  ; "Base__Result.ok_exn"
  ; "Python_lib__Defunc.apply"
  ; "Python_lib__Import.value_error"
  ; "Python_lib__Py_module.set"
  ; "Python_lib__Py_module.wrap_ocaml_errors"
  ; "Stdlib.failwith"
  ]
;;

let make_traceback backtrace =
  (* This returns the most recent function call at the beginning of the array. *)
  Stdlib.Printexc.backtrace_slots backtrace
  |> Option.value ~default:[||]
  |> Array.to_list
  |> List.filter_map ~f:(fun slot ->
    let function_name =
      Stdlib.Printexc.Slot.name slot |> Option.value ~default:"unknown"
    in
    if List.exists excluded_prefixes ~f:(fun prefix ->
      String.is_prefix function_name ~prefix)
    then None
    else (
      let filename =
        Stdlib.Printexc.Slot.location slot
        |> Option.value_map
             ~f:(fun loc -> loc.Stdlib.Printexc.filename)
             ~default:"unknown"
      in
      let line_number =
        Stdlib.Printexc.Slot.location slot
        |> Option.value_map ~f:(fun loc -> loc.Stdlib.Printexc.line_number) ~default:42
      in
      let function_name = Printf.sprintf "%s:%d" function_name line_number in
      Some { Py.Traceback.filename; function_name; line_number }))
;;

let raise_py_err_with_backtrace ?(unwrap_more = fun _ -> None) ?backtrace exn =
  let rec loop acc_traceback = function
    | Exn.Reraised (reraised, exn) ->
      let frame =
        { Py.Traceback.filename = "exn.ml"
        ; function_name = Printf.sprintf "reraise<%s>" reraised
        ; line_number = 0
        }
      in
      loop ([ frame ] :: acc_traceback) exn
    | exn ->
      (match unwrap_more exn with
       | None -> List.concat (List.rev acc_traceback), exn
       | Some (backtraces, exn) ->
         loop (List.map ~f:make_traceback backtraces @ acc_traceback) exn)
  in
  let additional_traceback, exn = loop [] exn in
  (* The [additional_traceback] information is from function calls less recent than the
     ones from [Backtrace.Exn.most_recent] *)
  let traceback =
    Option.value_map backtrace ~f:make_traceback ~default:[] @ additional_traceback
  in
  let py_err, msg, original_traceback =
    match exn with
    | Py.E _ -> raise exn
    | Py.Err (py_err, msg) -> py_err, msg, []
    | Py.Err_with_traceback (py_err, msg, traceback) -> py_err, msg, traceback
    | Failure s -> ValueError, s, []
    | exn -> ValueError, Exn.to_string_mach exn, []
  in
  raise (Py.Err_with_traceback (py_err, msg, original_traceback @ traceback))
;;

let wrap_ocaml_errors f =
  try f () with
  | Py.(E _ | Err _ | Err_with_traceback _) as pyerr ->
    (* We do not include the latest backtrace information here as the code raising these
       exceptions is assumed to have put enough context in it and the backtrace leading
       to these is often noisy. *)
    raise pyerr
  | exn -> raise_py_err_with_backtrace exn ~backtrace:(Backtrace.Exn.most_recent ())
;;

let set_function t ?docstring name fn =
  let fn args = wrap_ocaml_errors (fun () -> fn args) in
  set_value t name (Py.Callable.of_function ~name ?docstring fn)
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
  set_value t name (Py.Callable.of_function_with_keywords ~name ?docstring fn)
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
  let set modl = set { modl; module_name = "<anon>" }
end

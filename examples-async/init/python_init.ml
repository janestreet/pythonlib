(* Hacky way to trigger a dependency to the PyInit_ocaml_async function. *)
external pyinit : unit -> unit = "PyInit_ocaml_async"

let () = ignore (pyinit : unit -> unit)

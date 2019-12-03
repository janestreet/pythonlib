(* Hacky way to trigger a dependency to the PyInit_ocaml function. *)
external pyinit : unit -> unit = "PyInit_ocaml"

let () = ignore (pyinit : unit -> unit)

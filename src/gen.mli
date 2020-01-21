(** This can be used to auto-generated python<->ocaml bindings based on a cmi file.

    Take a look at ../examples-gen/generated for an example.
*)

open Base

(** [write_ml] writes the actual ml file containing the bindings. (E.g. in the above
    example it generates date_bindings.ml).

    Note, this returns a list of all the types that are needed. *)
val write_ml
  :  Stdio.Out_channel.t
  -> Cmi_format.cmi_infos
  -> (Module_env.Path.t * string) Hash_set.t

(** [write_types] writes a separate file containing type conversion functions. It is
    expeected that the input to this function is the output of [write_ml] above. *)
val write_types : Stdio.Out_channel.t -> (Module_env.Path.t * string) Hash_set.t -> unit

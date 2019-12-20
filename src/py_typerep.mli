open Import
module Typerep = Typerep_lib.Std.Typerep

module Named_types : sig
  val register_exn
    :  name:string
    -> ocaml_type:string
    -> python_to_ocaml:(pyobject -> 'a)
    -> ocaml_to_python:('a -> pyobject)
    -> unit
end

(** [to_ocaml typerep] returns an ocaml string representing the type. *)
val to_ocaml : _ Typerep.t -> string

(** [ocaml_to_python typerep v] converts an arbitrary ocaml object with its typerep
    representation to a python value.
    See the implementation for supported constructions.
*)
val ocaml_to_python : 'a Typerep.t -> 'a -> pyobject

(** [python_to_ocaml typerep pybobject] converts a python object [pyobject] to an
    ocaml value according to some type description [typerep].
*)
val python_to_ocaml : 'a Typerep.t -> pyobject -> 'a

(** [parse str] parses an ocaml like string into a type representation.
    [str] cannot include any function.
*)
val parse : string -> Typerep.packed

(** [register_named_type ~name ~ocaml_type] registers a named type that can be used
    in [parse] with name [name].
    Values for this type will be type checked to match [ocaml_type] which should be
    a proper type annotation for the current toplevel, e.g. "t" if a type [t] is
    currently defined in the toplevel.
    A capsule is used to send and receive values to/from python.
*)
val register_named_type : name:string -> ocaml_type:string -> unit

val of_type : Type.t -> Typerep.packed

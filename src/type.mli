open Base

module Arg : sig
  type t = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string
end

type t =
  | Atom of Module_env.Path.t * string
  | Tuple2 of t * t
  | Tuple3 of t * t * t
  | Tuple4 of t * t * t * t
  | Tuple5 of t * t * t * t * t
  | Arrow of Arg.t * t * t
  | Apply of t * string

val uncurrify : t -> (Arg.t * t) list * t
val of_type_desc : Types.type_desc -> env:Module_env.t -> t Or_error.t
val to_string : t -> string
val contains_arrow : t -> bool

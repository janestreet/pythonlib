open Import
open Base

type t

val create : string -> t
val set_value : t -> string -> pyobject -> unit

val set_function
  :  t
  -> ?docstring:string
  -> string
  -> (pyobject array -> pyobject)
  -> unit

val set_function_with_keywords
  :  t
  -> ?docstring:string
  -> string
  -> (pyobject array -> (string, pyobject, String.comparator_witness) Map.t -> pyobject)
  -> unit

val set : t -> ?docstring:string -> string -> pyobject Defunc.t -> unit
val set_unit : t -> ?docstring:string -> string -> unit Defunc.t -> unit

(* Helper function to get keywords from a python object.
   When no keyword is present, null is used; otherwise a
   python dictionary with string key gets used.
*)

val keywords_of_python
  :  pyobject
  -> (string, pyobject, String.comparator_witness) Map.t Or_error.t

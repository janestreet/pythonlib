open Base
open Import

type _ t

val wrap : 'a t -> 'a -> pyobject
val unwrap_exn : 'a t -> pyobject -> 'a
val unwrap : 'a t -> pyobject -> 'a option

module Init : sig
  type 'a cls = 'a t
  type 'a t

  val create : ?docstring:string -> ('a cls -> args:pyobject list -> 'a) -> 'a t
end

module Method : sig
  type 'a cls = 'a t
  type 'a t

  val create
    :  ?docstring:string
    -> string
    (** In the [methods] callbacks, [self] contains both the embeded ocaml
        value as well as the Python wrapper object. *)
    -> ('a cls -> self:'a * pyobject -> args:pyobject list -> pyobject)
    -> 'a t

  val create_raw
    :  ?docstring:string
    -> string
    (** In the raw callbacks, [self] contains only the Python wrapper object. *)
    -> ('a cls -> self:pyobject -> args:pyobject list -> pyobject)
    -> 'a t

  val create_with_keywords
    :  ?docstring:string
    -> string
    -> ('a cls
        -> self:'a * pyobject
        -> args:pyobject list
        -> keywords:(string, pyobject, String.comparator_witness) Map.t
        -> pyobject)
    -> 'a t

  val defunc
    :  ?docstring:string
    -> string
    -> ('a cls -> self:'a * pyobject -> pyobject) Defunc.t
    -> 'a t

  val no_arg
    :  ?docstring:string
    -> string
    -> ('a cls -> self:'a * pyobject -> pyobject)
    -> 'a t
end

val make
  :  ?to_string_repr:('a t -> 'a -> string)
  -> ?to_string:('a t -> 'a -> string)
  -> ?eq:('a t -> 'a -> 'a -> bool)
  -> ?init:'a Init.t
  -> string
  -> methods:'a Method.t list
  -> 'a t

val register_in_module : 'a t -> Py_module.t -> unit
val clear_content : 'a t -> pyobject -> unit
val set_content : 'a t -> pyobject -> 'a -> unit
val cls_object : 'a t -> pyobject
val is_instance : 'a t -> pyobject -> bool

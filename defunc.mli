open Base
open Import

type _ t

module Of_python : sig
  type 'a t

  val create : type_name:string -> conv:(pyobject -> 'a) -> 'a t
end

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t

val params_docstring : 'a t -> string option

val apply
  :  'a t
  -> pyobject array
  -> (string, pyobject, String.comparator_witness) Map.t
  -> 'a

module Param : sig
  val positional : string -> 'a Of_python.t -> docstring:string -> 'a t
  val keyword : ?default:'a -> string -> 'a Of_python.t -> docstring:string -> 'a t
  val keyword_opt : string -> 'a Of_python.t -> docstring:string -> 'a option t
  val int : int Of_python.t
  val float : float Of_python.t
  val bool : bool Of_python.t
  val string : string Of_python.t
  val pyobject : pyobject Of_python.t
  val pair : 'a Of_python.t -> 'b Of_python.t -> ('a * 'b) Of_python.t

  val triple
    :  'a Of_python.t
    -> 'b Of_python.t
    -> 'c Of_python.t
    -> ('a * 'b * 'c) Of_python.t

  val list : 'a Of_python.t -> 'a list Of_python.t
  val one_or_tuple_or_list : 'a Of_python.t -> 'a list Of_python.t
  val dict : key:'a Of_python.t -> value:'b Of_python.t -> ('a * 'b) list Of_python.t
end

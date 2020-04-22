open Base
open Import

type _ t

module Of_python : sig
  type 'a t = private
    { type_name : string
    ; conv : pyobject -> 'a
    }

  val create : type_name:string -> conv:(pyobject -> 'a) -> 'a t
end

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t

val params_docstring : ?docstring:string -> 'a t -> string
val no_arg : (unit -> 'a) -> 'a t

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
  val typerep : 'a Typerep_lib.Std.Typerep.t -> 'a Of_python.t
  val pyobject : pyobject Of_python.t
  val pair : 'a Of_python.t -> 'b Of_python.t -> ('a * 'b) Of_python.t

  val triple
    :  'a Of_python.t
    -> 'b Of_python.t
    -> 'c Of_python.t
    -> ('a * 'b * 'c) Of_python.t

  val quadruple
    :  'a Of_python.t
    -> 'b Of_python.t
    -> 'c Of_python.t
    -> 'd Of_python.t
    -> ('a * 'b * 'c * 'd) Of_python.t

  val quintuple
    :  'a Of_python.t
    -> 'b Of_python.t
    -> 'c Of_python.t
    -> 'd Of_python.t
    -> 'e Of_python.t
    -> ('a * 'b * 'c * 'd * 'e) Of_python.t

  val option : 'a Of_python.t -> 'a option Of_python.t
  val list : 'a Of_python.t -> 'a list Of_python.t
  val list_or_iter : 'a Of_python.t -> 'a list Of_python.t
  val one_or_tuple_or_list : 'a Of_python.t -> 'a list Of_python.t

  (* [one_or_tuple_or_list_relaxed] can be used to allow individual items in a list to
     be invalid. *)
  val one_or_tuple_or_list_relaxed : 'a Of_python.t -> 'a Or_error.t list Of_python.t
  val dict : key:'a Of_python.t -> value:'b Of_python.t -> ('a * 'b) list Of_python.t
  val star_args : docstring:string -> pyobject list t

  val star_kwargs
    :  docstring:string
    -> (string, pyobject, String.comparator_witness) Map.t t
end

open Base
open Import

type _ t

module Of_python : sig
  type 'a t = private
    { type_name : string
    ; conv : pyobject -> 'a
    }

  val create : type_name:string -> conv:(pyobject -> 'a) -> 'a t

  (** [map t ~f] has the same [type_name] as [t]. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

include Applicative.S with type 'a t := 'a t
include Applicative.Let_syntax with type 'a t := 'a t

val params_docstring : ?docstring:string -> 'a t -> string
val no_arg : (unit -> 'a) -> 'a t

(* Be cautious when using [apply_] that it does not ensure that all the parameters
   have been used *before* evaluating the wrapped function. *)
val apply_
  :  'a t
  -> pyobject array
  -> (string, pyobject, String.comparator_witness) Map.t
  -> 'a

val apply
  :  (unit -> 'a) t
  -> pyobject array
  -> (string, pyobject, String.comparator_witness) Map.t
  -> 'a

module Param : sig
  (** [choice x y] first attempts conversion using x.conv. if that fails, it attempts y.conv *)
  val choice : 'a Of_python.t -> 'b Of_python.t -> ('a, 'b) Either.t Of_python.t

  val choice' : 'a Of_python.t -> 'a Of_python.t -> 'a Of_python.t
  val map : 'a Of_python.t -> f:('a -> 'b) -> 'b Of_python.t
  val positional_only : string -> 'a Of_python.t -> docstring:string -> 'a t

  val positional_or_keyword
    :  ?default:'a
    -> string
    -> 'a Of_python.t
    -> docstring:string
    -> 'a t

  val keyword : ?default:'a -> string -> 'a Of_python.t -> docstring:string -> 'a t
  val int : int Of_python.t

  (** [int_sequence_arg] converts integer sequences in a fast way if possible, e.g. going
      via numpy and bigarray if possible *)
  val int_sequence_arg : int array Of_python.t

  val float : float Of_python.t

  (** [float_sequence_arg] converts float sequences in a fast way if possible, e.g. going
      via numpy and bigarray if possible *)
  val float_sequence_arg : float array Of_python.t

  val bool : bool Of_python.t
  val char : char Of_python.t
  val string : string Of_python.t
  val callable : (pyobject array -> pyobject) Of_python.t
  val path : string Of_python.t
  val typerep : 'a Typerep_lib.Std.Typerep.t -> 'a Of_python.t

  (** WARNING: Do not use [pyobject] together with [Broadcast.t] - if you do, every value
      passed to the relevant python argument will be interpreted as a constant for the
      purposes of the broadcast, even if the value is a list or pandas series, which is
      probably not what you want. *)
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
  val array_or_iter : 'a Of_python.t -> 'a array Of_python.t
  val list_or_iter : 'a Of_python.t -> 'a list Of_python.t

  (* 'a should not be encoded as a python tuple, list or none. *)
  val one_or_tuple_or_list : 'a Of_python.t -> 'a list Of_python.t

  (* [one_or_tuple_or_list_relaxed] can be used to allow individual items in a list to
     be invalid. *)
  val one_or_tuple_or_list_relaxed : 'a Of_python.t -> 'a Or_error.t list Of_python.t
  val with_broadcast : 'a Of_python.t -> arg_name:string -> 'a Broadcast.t Of_python.t

  val positional_or_keyword_broadcast
    :  string
    -> 'a Of_python.t
    -> docstring:string
    -> 'a Broadcast.t t

  (** Convenience wrapper around [keyword] and [with_broadcast] that ensures the same
      [arg_name] is passed to each. *)
  val keyword_broadcast
    :  ?default:'a
    -> string
    -> 'a Of_python.t
    -> docstring:string
    -> 'a Broadcast.t t

  val keyword_opt_broadcast
    :  string
    -> 'a Of_python.t
    -> docstring:string
    -> 'a Broadcast.t option t

  (** Works just like [keyword_opt_broadcast] but uses [keyword] instead of [keyword_opt],
      which is deprecated. In practice, this means that this will treat [None] the same as
      not passing in the arg, instead of failing. *)
  val keyword_opt_broadcast'
    :  string
    -> 'a Of_python.t
    -> docstring:string
    -> 'a Broadcast.t option t

  val dict : key:'a Of_python.t -> value:'b Of_python.t -> ('a * 'b) list Of_python.t
  val star_args : docstring:string -> pyobject list t

  val star_kwargs
    :  docstring:string
    -> (string, pyobject, String.comparator_witness) Map.t t

  (* A numpy array, note that the memory is shared between the ocaml and python sides. *)
  val numpy_array
    :  ('a, 'b) Bigarray.kind
    -> 'c Bigarray.layout
    -> ('a, 'b, 'c) Bigarray.Genarray.t Of_python.t

  val numpy_array1
    :  ('a, 'b) Bigarray.kind
    -> 'c Bigarray.layout
    -> ('a, 'b, 'c) Bigarray.Array1.t Of_python.t

  val numpy_array2
    :  ('a, 'b) Bigarray.kind
    -> 'c Bigarray.layout
    -> ('a, 'b, 'c) Bigarray.Array2.t Of_python.t

  val numpy_array3
    :  ('a, 'b) Bigarray.kind
    -> 'c Bigarray.layout
    -> ('a, 'b, 'c) Bigarray.Array3.t Of_python.t
end

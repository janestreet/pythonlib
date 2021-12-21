(* A module to handle argument broadcasting.
   Note that this does not support list of lists or series of lists properly.
*)

open! Base
open! Import

type 'a t

include Applicative.S with type 'a t := 'a t

val create : pyobject -> (pyobject -> 'a) -> arg_name:string -> 'a t
val constant : 'a -> 'a t
val many : 'a list -> arg_name:string -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t

(** NOTE: [merge] flips the order of its arguments. This is to reflect it's intended usage
    in pipelines via [|>]. *)
val merge : 'a t -> 'b t -> ('b * 'a) t

val zip2 : 'a t -> 'b t -> ('a * 'b) t
val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val to_list : 'a t -> 'a list
val python_of_t : 'a t -> 'b list -> to_python:('b -> pyobject) -> pyobject

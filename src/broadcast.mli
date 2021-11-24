(* A module to handle argument broadcasting.
   Note that this does not support list of lists or series of lists properly.
*)

open! Base
open! Import

module Arg : sig
  type 'a t =
    { name : string
    ; pyobject : pyobject
    ; of_python : pyobject -> 'a
    }
end

type 'a t

val one : 'a Arg.t -> 'a t
val many : 'a list -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val merge : 'a t * string -> 'b t -> ('b * 'a) t
val zip2 : 'a Arg.t -> 'b Arg.t -> ('a * 'b) t
val zip3 : 'a Arg.t -> 'b Arg.t -> 'c Arg.t -> ('a * 'b * 'c) t
val zip4 : 'a Arg.t -> 'b Arg.t -> 'c Arg.t -> 'd Arg.t -> ('a * 'b * 'c * 'd) t
val to_list : 'a t -> 'a list
val python_of_t : 'a t -> 'b list -> to_python:('b -> pyobject) -> pyobject

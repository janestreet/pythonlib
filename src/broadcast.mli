(* A module to handle argument broadcasting.
   Note that this does not support list of lists or series of lists properly.
*)

open! Base
open! Import

type 'a t

module Open_on_rhs_intf : sig
  module type S = Applicative.S with type 'a t := 'a t
end

include Applicative.S with type 'a t := 'a t

include
  Applicative.Let_syntax
    with type 'a t := 'a t
     and module Open_on_rhs_intf := Open_on_rhs_intf

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
val python_of_t' : 'a t -> to_python:('a -> pyobject) -> pyobject

(** Some if [t] was created from a pandas series, else [None]. *)
val index : _ t -> pyobject option

(** Creates a dataframe that maintains the original indexing of [t].

    This effectively calls pandas.DataFrame(data, index, **kwargs) where index is the
    index of [t].
*)
val df_of_t : _ t -> data:pyobject -> kwargs:(string * pyobject) list -> pyobject

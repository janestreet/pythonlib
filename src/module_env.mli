module Path : sig
  type t

  val empty : t
  val append : t -> string -> t
  val names : t -> string list
end

type t

val create : unit -> t
val path : t -> Path.t
val enter_module : t -> module_ident:Ident.t -> t
val add_type : t -> type_ident:Ident.t -> unit
val find_type : t -> type_ident:Ident.t -> Path.t option
val find_module : t -> module_ident:Ident.t -> Path.t option

open Base

module Path : sig
  type t

  val empty : t
  val append : t -> string -> t
  val names : t -> string list
end = struct
  type t = string list

  let empty = []
  let append t v = v :: t
  let names t = List.rev t
end

type t =
  { path : Path.t
  ; types : unit Ident.Tbl.t
  ; modules : unit Ident.Tbl.t
  ; parent : t option
  }

let path t = t.path

let create () =
  { path = Path.empty
  ; types = Ident.Tbl.create 4
  ; modules = Ident.Tbl.create 4
  ; parent = None
  }
;;

let enter_module t ~module_ident =
  let module_t =
    { path = Path.append t.path (Ident.name module_ident)
    ; types = Ident.Tbl.create 4
    ; modules = Ident.Tbl.create 4
    ; parent = Some t
    }
  in
  Ident.Tbl.replace t.modules module_ident ();
  module_t
;;

let add_type t ~type_ident = Ident.Tbl.replace t.types type_ident ()

let find_type t ~type_ident =
  let rec walk t =
    if Ident.Tbl.mem t.types type_ident
    then Some t.path
    else (
      match t.parent with
      | None -> None
      | Some parent -> walk parent)
  in
  walk t
;;

let find_module t ~module_ident =
  let rec walk t =
    if Ident.Tbl.mem t.modules module_ident
    then Some t.path
    else (
      match t.parent with
      | None -> None
      | Some parent -> walk parent)
  in
  walk t
;;

let%expect_test "module-env" =
  let path_to_string path = Path.names path |> String.concat ~sep:"." in
  let t = create () in
  let ident_t = Ident.create_local "t" in
  let ident_t2 = Ident.create_local "t" in
  let ident_u = Ident.create_local "u" in
  let ident_v = Ident.create_local "v" in
  let ident_w = Ident.create_local "w" in
  let module_ident = Ident.create_local "module-ident" in
  add_type t ~type_ident:ident_t;
  add_type t ~type_ident:ident_u;
  add_type t ~type_ident:ident_v;
  let submodule_t = enter_module t ~module_ident in
  add_type submodule_t ~type_ident:ident_t2;
  add_type submodule_t ~type_ident:ident_u;
  let find_and_print ident =
    match find_type submodule_t ~type_ident:ident with
    | None -> Stdio.printf "cannot find %s\n" (Ident.name ident)
    | Some path -> Stdio.printf "found %s.%s\n" (path_to_string path) (Ident.name ident)
  in
  find_and_print ident_t;
  find_and_print ident_t2;
  find_and_print ident_u;
  find_and_print ident_v;
  find_and_print ident_w;
  [%expect
    {|
    found .t
    found module-ident.t
    found module-ident.u
    found .v
    cannot find w
      |}]
;;

open Base
open Poly
include Ppx_python_runtime


type pyobject = Pytypes.pyobject

let python_of_pyobject = Fn.id
let pyobject_of_python = Fn.id

module Convert_as_string (M : Stringable.S) = struct
  let python_of_t t = M.to_string t |> python_of_string
  let t_of_python p = string_of_python p |> M.of_string
end

let get_class p =
  Option.bind (Py.Object.get_attr_string p "__class__") ~f:(fun cls ->
    Option.map (Py.Object.get_attr_string cls "__name__") ~f:Py.String.to_string)
;;

module One_or_tuple = struct
  (* 'a should not be encoded as a python tuple or none! *)
  type 'a t = 'a list

  let python_of_t python_of_a t =
    match t with
    | [] -> Py.none
    | [ v ] -> python_of_a v
    | vs -> Py.Tuple.of_list_map python_of_a vs
  ;;

  let t_of_python a_of_python p =
    try [ a_of_python p ] with
    | _ ->
      if p = Py.none
      then []
      else if Py.Tuple.check p
      then Py.Tuple.to_list_map a_of_python p
      else failwith "incorrect python type"
  ;;
end

let is_list_type p = Py.List.check p || Py.Iter.check p

module One_or_tuple_or_list = struct
  (* 'a should not be encoded as a python tuple, list or none! *)
  type 'a t = 'a list

  let python_of_t = One_or_tuple.python_of_t

  let t_of_python a_of_python p =
    try One_or_tuple.t_of_python a_of_python p with
    | _ ->
      if is_list_type p
      then Py.List.to_list_map a_of_python p
      else failwith "incorrect python type"
  ;;
end

module Or_error_python = struct
  type err = { caml_error : string } [@@deriving python]
  type 'a t = 'a Or_error.t

  let t_of_python ok_of_python p =
    match ok_of_python p with
    | v -> Ok v
    | exception exn -> Or_error.of_exn exn
  ;;

  let python_of_t python_of_a t =
    match t with
    | Ok a -> python_of_a a
    | Error err -> python_of_err { caml_error = Error.to_string_hum err }
  ;;
end

let value_errorf fmt = Printf.ksprintf (fun msg -> raise (Py.Err (ValueError, msg))) fmt

module One_or_tuple_or_list_or_error = struct
  type 'a t = 'a Or_error_python.t list

  let python_of_t = One_or_tuple_or_list.python_of_t

  let t_of_python a_of_python p ~type_name =
    match One_or_tuple.t_of_python a_of_python p with
    | v -> List.map v ~f:(fun v -> Ok v)
    | exception _ ->
      if is_list_type p
      then
        Py.List.to_list_map
          (fun p ->
             Or_error_python.t_of_python a_of_python p
             |> Or_error.tag ~tag:("trying to parse as " ^ type_name))
          p
      else failwith "incorrect python type"
  ;;
end

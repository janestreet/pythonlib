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

module One_or_tuple_or_list = struct
  (* 'a should not be encoded as a python tuple, list or none! *)
  type 'a t = 'a list

  let python_of_t = One_or_tuple.python_of_t

  let t_of_python a_of_python p =
    try One_or_tuple.t_of_python a_of_python p with
    | _ ->
      if Py.List.check p
      then Py.List.to_list_map a_of_python p
      else failwith "incorrect python type"
  ;;
end

module Or_error_python = struct
  type err = { caml_error : string } [@@deriving python]
  type 'a t = 'a Or_error.t

  let t_of_python _ _ = failwith "cannot extract Or_error_python from python values"

  let python_of_t python_of_a t =
    match t with
    | Ok a -> python_of_a a
    | Error err -> python_of_err { caml_error = Error.to_string_hum err }
  ;;
end

let value_errorf fmt = Printf.ksprintf (fun msg -> raise (Py.Err (ValueError, msg))) fmt

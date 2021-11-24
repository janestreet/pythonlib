open! Base
open! Import

module Arg = struct
  type 'a t =
    { name : string
    ; pyobject : pyobject
    ; of_python : pyobject -> 'a
    }
end

(* Note that [Single_value foo] and [List [foo]] are not the same thing, in the
   first case only a single value was passed, and in the second case a list of
   length 1 was passed.
   This is helpful so that the functions can return a single value in this case.
*)
type 'a t =
  | Single_value of 'a
  | List of 'a list
  | Series of
      { values : 'a list
      ; index : pyobject
      }

let map t ~f =
  match t with
  | Single_value v -> Single_value (f v)
  | List vs -> List (List.map vs ~f)
  | Series { values; index } -> Series { values = List.map values ~f; index }
;;

let to_list = function
  | Single_value a -> [ a ]
  | List values | Series { values; _ } -> values
;;

let one { Arg.name = _; pyobject; of_python } =
  let map pyobject = Py.List.to_list_map of_python pyobject in
  if Py.List.check pyobject
  then List (map pyobject)
  else (
    match Lazy.force pd_series with
    | Some pd_series when Py.Object.is_instance pyobject pd_series ->
      let values =
        Option.value_exn (Py.Object.get_attr_string pyobject "values") |> map
      in
      let index = Option.value_exn (Py.Object.get_attr_string pyobject "index") in
      Series { values; index }
    | Some _ | None -> Single_value (of_python pyobject))
;;

let many vs = List vs

let merge (t2, name) t1 =
  let zip_exn vs1 vs2 =
    let n1 = List.length vs1 in
    let n2 = List.length vs2 in
    if n1 <> n2
    then value_errorf "unexpected number of values for %s: %d <> %d" name n2 n1;
    List.zip_exn vs1 vs2
  in
  match t1, t2 with
  | Single_value v1, Single_value v2 -> Single_value (v1, v2)
  | Single_value v1, List vs2 -> List (List.map vs2 ~f:(fun v2 -> v1, v2))
  | List vs1, Single_value v2 -> List (List.map vs1 ~f:(fun v1 -> v1, v2))
  | List vs1, List vs2 -> List (zip_exn vs1 vs2)
  | Single_value v1, Series { values = vs2; index } ->
    Series { values = List.map vs2 ~f:(fun v2 -> v1, v2); index }
  | Series { values = vs1; index }, Single_value v2 ->
    Series { values = List.map vs1 ~f:(fun v1 -> v1, v2); index }
  | Series { values = vs1; index }, List vs2 -> Series { values = zip_exn vs1 vs2; index }
  | List vs1, Series { values = vs2; index } -> Series { values = zip_exn vs1 vs2; index }
  | Series { values = vs1; index = i1 }, Series { values = vs2; index = i2 } ->
    if not (Py.Object.call_method i1 "equals" [| i2 |] |> bool_of_python)
    then value_errorf "unexpected series index for %s" name;
    Series { values = zip_exn vs1 vs2; index = i1 }
;;

let merge_arg arg t = merge (one arg, arg.name) t
let zip2 arg1 arg2 = one arg1 |> merge_arg arg2

let zip3 arg1 arg2 arg3 =
  one arg1
  |> merge_arg arg2
  |> merge_arg arg3
  |> map ~f:(fun ((v1, v2), v3) -> v1, v2, v3)
;;

let zip4 arg1 arg2 arg3 arg4 =
  one arg1
  |> merge_arg arg2
  |> merge_arg arg3
  |> merge_arg arg4
  |> map ~f:(fun (((v1, v2), v3), v4) -> v1, v2, v3, v4)
;;

let python_of_t t values ~to_python =
  let same_length_exn vs1 vs2 =
    let n1 = List.length vs1 in
    let n2 = List.length vs2 in
    if n1 <> n2
    then value_errorf "internal error, unexpected number of outputs %d <> %d" n2 n1
  in
  match t with
  | Single_value _ ->
    (match values with
     | [ v ] -> to_python v
     | _ ->
       value_errorf "internal error, expected a single output got %d" (List.length values))
  | List l ->
    same_length_exn l values;
    Py.List.of_list_map to_python values
  | Series { values = l; index } ->
    same_length_exn l values;
    let pd_series = Option.value_exn (Lazy.force pd_series) in
    Py.Callable.to_function pd_series [| Py.List.of_list_map to_python values; index |]
;;

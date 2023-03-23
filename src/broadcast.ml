open! Base
open! Import

(* The regular non-empty list module depends on core, which we don't want to do, so we
   define our own version here. *)
module Nonempty_list = struct
  type 'a t = ( :: ) of 'a * 'a list

  let to_list (hd :: tl) : _ list = hd :: tl
  let append (hd :: tl) t = hd :: (tl @ to_list t)
end

module T = struct
  (* Note that [Single_value foo] and [List_or_series [foo]] are not the same thing, in
     the first case only a single value was passed, and in the second case a list of
     length 1 was passed.
     This is helpful so that the functions can return a single value in this case.

     Note also that the purpose of [list_arg_names] is to solely print informative error
     messages. Because [Single_value]s can never be involved in causing errors, we don't
     store arg names for those. This also means that, for a [t] constructed via merging,
     [list_arg_names] does NOT contain the arg names of ALL [t]s used to produce it -
     instead, it wll only contain the arg names of [List_or_series] values which were
     merged.
  *)

  type 'a t =
    | Single_value of 'a
    | List_or_series of
        { values : 'a list
        ; list_arg_names : string Nonempty_list.t
        ; index : pyobject option
        (* This is [Some _] if [t] was created from a pandas series, else [None]. *)
        }

  let map t ~f =
    match t with
    | Single_value v -> Single_value (f v)
    | List_or_series { values; list_arg_names; index } ->
      List_or_series { values = List.map values ~f; list_arg_names; index }
  ;;

  let merge t2 t1 =
    let str_of_arg_names arg_names =
      Nonempty_list.to_list arg_names
      |> List.map ~f:(fun str -> "'" ^ str ^ "'")
      |> String.concat ~sep:","
    in
    let zip_exn (vs1, list_arg_names1) (vs2, list_arg_names2) =
      let n1 = List.length vs1 in
      let n2 = List.length vs2 in
      if n1 <> n2
      then
        value_errorf
          "mismatching number of values: argument(s) %s had %d values, while argument(s) \
           %s had %d values"
          (str_of_arg_names list_arg_names1)
          n1
          (str_of_arg_names list_arg_names2)
          n2;
      let values = List.zip_exn vs1 vs2 in
      let list_arg_names = Nonempty_list.append list_arg_names1 list_arg_names2 in
      values, list_arg_names
    in
    match t1, t2 with
    | Single_value v1, Single_value v2 -> Single_value (v1, v2)
    | Single_value v1, List_or_series { values = vs2; list_arg_names; index } ->
      List_or_series
        { values = List.map vs2 ~f:(fun v2 -> v1, v2); list_arg_names; index }
    | List_or_series { values = vs1; list_arg_names; index }, Single_value v2 ->
      List_or_series
        { values = List.map vs1 ~f:(fun v1 -> v1, v2); list_arg_names; index }
    | ( List_or_series { values = vs1; list_arg_names = list_arg_names1; index = i1 }
      , List_or_series { values = vs2; list_arg_names = list_arg_names2; index = i2 } ) ->
      let index =
        match i1, i2 with
        | Some i1, Some i2 ->
          if not (Py.Object.call_method i1 "equals" [| i2 |] |> bool_of_python)
          then
            value_errorf
              "series indexes for argument(s) %s and %s differ"
              (str_of_arg_names list_arg_names1)
              (str_of_arg_names list_arg_names2)
          else Some i1
        | (Some _ as i), None | None, (Some _ as i) -> i
        | None, None -> None
      in
      let values, list_arg_names =
        zip_exn (vs1, list_arg_names1) (vs2, list_arg_names2)
      in
      List_or_series { values; list_arg_names; index }
  ;;

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return x = Single_value x
      let apply t_f t = merge t t_f |> map ~f:(fun (f, x) -> f x)
      let map = `Custom map
    end)
end

include T

module Open_on_rhs_intf = struct
  module type S = Applicative.S with type 'a t := 'a t
end

include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)

let to_list = function
  | Single_value a -> [ a ]
  | List_or_series { values; _ } -> values
;;

let create pyobject of_python ~arg_name =
  let map pyobject = Py.List.to_list_map of_python pyobject in
  if Py.List.check pyobject
  then
    List_or_series { values = map pyobject; list_arg_names = [ arg_name ]; index = None }
  else (
    match Lazy.force pd_series with
    | Some pd_series when Py.Object.is_instance pyobject pd_series ->
      (* Make sure to use "Series.array" instead of "Series.values". Because, given a
         series of datetimes, calling df['start'].values produces an array of datetime64s.
         However, accessing each element directly (e.g. df['start'][0] ) gives us
         Timestamp which contains timezone information.

         Relevant links:
         https://stackoverflow.com/questions/21989286/why-pandas-series-return-the-element-of-my-numpy-datetime64-array-as-timestamp
         https://pandas.pydata.org/docs/reference/api/pandas.Series.values.html

      *)
      let values = Option.value_exn (Py.Object.get_attr_string pyobject "array") |> map in
      let index = Some (Option.value_exn (Py.Object.get_attr_string pyobject "index")) in
      List_or_series { values; list_arg_names = [ arg_name ]; index }
    | Some _ | None -> Single_value (of_python pyobject))
;;

let constant x = Single_value x

let many values ~arg_name =
  List_or_series { values; list_arg_names = [ arg_name ]; index = None }
;;

let zip2 arg1 arg2 = arg1 |> merge arg2

let zip3 arg1 arg2 arg3 =
  arg1 |> merge arg2 |> merge arg3 |> map ~f:(fun ((v1, v2), v3) -> v1, v2, v3)
;;

let zip4 arg1 arg2 arg3 arg4 =
  arg1
  |> merge arg2
  |> merge arg3
  |> merge arg4
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
  | List_or_series { values = l; index; list_arg_names = _ } ->
    same_length_exn l values;
    (match index with
     | None -> Py.List.of_list_map to_python values
     | Some index ->
       let pd_series = Option.value_exn (Lazy.force pd_series) in
       Py.Callable.to_function pd_series [| Py.List.of_list_map to_python values; index |])
;;

let python_of_t' values ~to_python = python_of_t values (to_list values) ~to_python

let df_of_t t ~data ~kwargs =
  let index =
    match t with
    | Single_value _ -> None
    | List_or_series { values = _; index; list_arg_names = _ } -> index
  in
  let df = Lazy.force pd_dataframe in
  match df, index with
  | None, _ -> data
  | Some df, None ->
    (* The following is equivalent to pandas.Dataframe(data, index=None, **kwargs) *)
    Py.Callable.to_function_with_keywords df [| data |] kwargs
  | Some df, Some index ->
    (* The following is equivalent to pandas.Dataframe(data, index, **kwargs) *)
    Py.Callable.to_function_with_keywords df [| data; index |] kwargs
;;

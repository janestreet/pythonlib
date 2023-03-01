open Base
open Poly
include Ppx_python_runtime


type pyobject = Pytypes.pyobject

let python_of_pyobject = Fn.id
let pyobject_of_python = Fn.id

let py_list_to_container_map_safe f pyobject ~container_init =
  (* Use [Py.Sequence] rather than [Py.List] so that this works on both
     tuples and lists. *)
  container_init (Py.Sequence.length pyobject) ~f:(fun i ->
    Py.Sequence.get_item pyobject i |> Py.check_not_null |> f)
;;

let py_list_to_list_map_safe f pyobject =
  py_list_to_container_map_safe f pyobject ~container_init:(List.init :> _ -> f:_ -> _)
;;

let py_list_to_array_map_safe f pyobject =
  py_list_to_container_map_safe f pyobject ~container_init:(Array.init :> _ -> f:_ -> _)
;;

module Of_pythonable (Pythonable : sig
    type t [@@deriving python]
  end)
    (Conv : sig
       type pythonable
       type t

       val to_pythonable : t -> pythonable
       val of_pythonable : pythonable -> t
     end
     with type pythonable := Pythonable.t) : sig
  type t [@@deriving python]
end
with type t := Conv.t = struct
  let python_of_t t = Conv.to_pythonable t |> Pythonable.python_of_t
  let t_of_python pyobject = Pythonable.t_of_python pyobject |> Conv.of_pythonable
end

module Convert_as_string (M : Stringable.S) = struct
  let python_of_t t = M.to_string t |> python_of_string
  let t_of_python p = string_of_python p |> M.of_string
end

let get_class p =
  Option.bind (Py.Object.get_attr_string p "__class__") ~f:(fun cls ->
    Option.map (Py.Object.get_attr_string cls "__name__") ~f:Py.String.to_string)
;;

let value_error str = raise (Py.Err (ValueError, str))
let value_errorf fmt = Printf.ksprintf value_error fmt

let protect_python ~f =
  try f () with
  | Py.Err _ as pyerr -> raise pyerr
  | exn ->
    let msg = Printf.sprintf "ocaml error %s" (Exn.to_string_mach exn) in
    raise (Py.Err (ValueError, msg))
;;

let get_from_builtins =
  let cache = Hashtbl.create (module String) in
  fun str ->
    Hashtbl.find_or_add cache str ~default:(fun () ->
      match Py.Module.get (Py.Eval.get_builtins ()) str with
      | pyobject -> pyobject
      (* In some very specific conditions, the builtins module may contain some objects that
         are not in __main__.__builtins__, e.g. when using the %run macro in an
         ipython kernel.
      *)
      | exception _ -> Py.Module.get (Py.import "builtins") str)
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

let to_list p =
  let list = get_from_builtins "list" in
  Py.Object.call_function_obj_args list [| p |]
;;

let iterable_to_list p =
  if Py.List.check p
  then Some p
  else (
    match get_class p with
    | Some "Series" ->
      (* [Py.List.to_list] assumes the python object to follow the
         PySequence[1] protocol. Most importantly, it expects the python object
         to implement the PySequence_GetItem[2] C function which is used to
         access individual elements.

         [Py.List.to_list] fails for a Series object that is sliced to represent a non
         contiguous layout (such as by doing series[::2] in python). Because a pandas
         Series object does not follow the PySequence protocol, we cannot use the
         [Py.List] module to access its individual elements or iterate over it. Even in
         Python, we cannot use the construct series[i] (where i is the index) to access
         its individual elements. The recommended way to access individual elements in
         Python is to use its [iloc] or [at] attributes.

         It may help to know that iterating over sequence in python using the [for ... in
         ...] syntax works by first obtaining an iterator to that sequence using the
         python builtin function [iter].

         In OCaml, we can similarly use the [Py.Iter] module to iterate over the Series
         object, if we first obtain an iterator to it, using [Py.Object.get_iter]. This is
         one way we can interface with a pandas Series in OCaml. Some other alternatives
         include:

         1. converting to a python list (using the [Series.tolist] method)
         2. converting to a numpy array (using the [Series.to_numpy] method)
         3. converting to a pandas array (using the [Series.array] attribute)

         Our profiling results (as of Dec 4, 2019) indicate that converting to a python
         list is ~60% faster than converting to a numpy array (next best alternative).

         It was interesting to observe that [Py.List.to_list] works as expected for a
         Series that is not sliced. But fails for sliced series with non contiguous
         elements. However, for correctness guarantees and performance reasons, we went
         ahead with converting the series to a python list before calling
         [Py.List.to_list_map].

         References:

         [1] https://docs.python.org/3.6/c-api/sequence.html
         [2] https://docs.python.org/3.6/c-api/sequence.html#c.PySequence_GetItem
         [3] https://github.com/pandas-dev/pandas/issues/30042
      *)
      let p = Py.Module.get_function_with_keywords p "tolist" [||] [] in
      Some p
    | Some "ndarray" ->
      let p = Py.Module.get_function_with_keywords p "tolist" [||] [] in
      Some p
    | Some "range" -> to_list p |> Option.some
    | _ -> if Py.Iter.check p then to_list p |> Option.some else None)
;;

module One_or_tuple_or_list = struct
  (* 'a should not be encoded as a python tuple, list or none! *)
  type 'a t = 'a list

  let python_of_t = One_or_tuple.python_of_t

  let t_of_python a_of_python p =
    try One_or_tuple.t_of_python a_of_python p with
    | _ ->
      (match iterable_to_list p with
       | Some l -> py_list_to_list_map_safe a_of_python l
       | None -> failwith "incorrect python type")
  ;;
end

module Or_error_python = struct
  type 'a t = 'a Or_error.t

  let value_error_obj str =
    let value_error = get_from_builtins "ValueError" in
    Py.Object.call_function_obj_args value_error [| python_of_string str |]
  ;;

  let of_error pyobject =
    let pyexception = get_from_builtins "Exception" in
    if Py.Object.is_instance pyobject pyexception
    then
      Option.value_exn
        ~message:"no args field on python exception"
        (Py.Object.get_attr_string pyobject "args")
      |> list_of_python Py.Object.to_string
      |> String.concat ~sep:", "
      |> Option.some
    else None
  ;;

  let t_of_python ok_of_python p =
    match of_error p with
    | Some error -> Or_error.error_string error
    | None ->
      (match ok_of_python p with
       | v -> Ok v
       | exception exn -> Or_error.of_exn exn)
  ;;

  let python_of_t python_of_a t =
    match t with
    | Ok a -> python_of_a a
    | Error err -> Error.to_string_hum err |> value_error_obj
  ;;
end

module One_or_tuple_or_list_or_error = struct
  type 'a t = 'a Or_error_python.t list

  let python_of_t = One_or_tuple_or_list.python_of_t

  let t_of_python a_of_python p ~type_name =
    match One_or_tuple.t_of_python a_of_python p with
    | v -> List.map v ~f:(fun v -> Ok v)
    | exception _ ->
      (match iterable_to_list p with
       | Some p ->
         py_list_to_list_map_safe
           (fun p ->
              Or_error_python.t_of_python a_of_python p
              |> Or_error.tag ~tag:("trying to parse as " ^ type_name))
           p
       | None -> failwith "incorrect python type")
  ;;
end

let python_print str =
  let print = get_from_builtins "print" in
  Py.Object.call_function_obj_args print [| Py.String.of_string str |]
  |> (ignore : pyobject -> unit)
;;

let python_printf fmt = Printf.ksprintf python_print fmt

let python_eprint str =
  let print = get_from_builtins "print" in
  let stderr = Py.Module.get (Py.import "sys") "stderr" in
  Py.Callable.to_function_with_keywords
    print
    [| Py.String.of_string str |]
    [ "file", stderr ]
  |> (ignore : pyobject -> unit)
;;

let python_eprintf fmt = Printf.ksprintf python_eprint fmt
let builtins = lazy (Py.Module.builtins ())
let pandas = lazy (Option.try_with (fun () -> Py.Import.import_module "pandas"))
let numpy = lazy (Option.try_with (fun () -> Py.Import.import_module "numpy"))
let datetime = lazy (Option.try_with (fun () -> Py.Import.import_module "datetime"))
let pathlib = lazy (Py.Import.import_module "pathlib")
let path_cls = lazy (Py.Module.get (Lazy.force pathlib) "Path")

let pd_series =
  lazy (Lazy.force pandas |> Option.map ~f:(fun pd -> Py.Module.get pd "Series"))
;;

let pd_dataframe =
  lazy (Lazy.force pandas |> Option.map ~f:(fun pd -> Py.Module.get pd "DataFrame"))
;;

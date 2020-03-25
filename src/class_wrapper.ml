open Base
open Import

module Id : sig
  type t

  val create : unit -> t
  val to_string : t -> string
end = struct
  type t = int

  let create =
    let current = ref 0 in
    fun () ->
      Int.incr current;
      !current
  ;;

  let to_string = Int.to_string
end

let content_field = "_content"

type 'a t =
  { wrap : 'a -> Py.Object.t
  ; unwrap : Py.Object.t -> 'a
  ; name : string
  ; mutable cls_object : Py.Object.t option
  }

let set_cls_object_exn t pyobject =
  if Option.is_some t.cls_object
  then Printf.failwithf "cls_object for %s has already been set" t.name ();
  t.cls_object <- Some pyobject
;;

module Init = struct
  type 'a cls = 'a t

  type 'a t =
    { fn : 'a cls -> args:pyobject list -> 'a
    ; docstring : string option
    }

  let create ?docstring fn = { docstring; fn }
end

module Method = struct
  type 'a cls = 'a t

  type 'a fn =
    | No_keywords of ('a cls -> self:'a * pyobject -> args:pyobject list -> pyobject)
    | No_keywords_raw of ('a cls -> self:pyobject -> args:pyobject list -> pyobject)
    | With_keywords of
        ('a cls
         -> self:'a * pyobject
         -> args:pyobject list
         -> keywords:(string, pyobject, String.comparator_witness) Map.t
         -> pyobject)

  type 'a t =
    { name : string
    ; fn : 'a fn
    ; docstring : string option
    }

  let create ?docstring name fn = { name; fn = No_keywords fn; docstring }
  let create_raw ?docstring name fn = { name; fn = No_keywords_raw fn; docstring }

  let create_with_keywords ?docstring name fn =
    { name; fn = With_keywords fn; docstring }
  ;;

  let defunc ?docstring name defunc =
    let docstring = Defunc.params_docstring ?docstring defunc in
    let fn cls ~self ~args ~keywords =
      let fn = Defunc.apply defunc (Array.of_list args) keywords in
      fn cls ~self
    in
    create_with_keywords ~docstring name fn
  ;;

  let no_arg ?docstring name fn =
    let fn cls ~self ~args ~keywords =
      if not (List.is_empty args) then value_errorf "no argument expected";
      if not (Map.is_empty keywords) then value_errorf "no keyword argument expected";
      fn cls ~self
    in
    create_with_keywords ?docstring name fn
  ;;
end

let wrap_capsule t obj = t.wrap obj

let unwrap_exn t pyobj =
  let pyobj =
    match Py.Object.get_attr_string pyobj content_field with
    | None -> Printf.failwithf "no %s field in object" content_field ()
    | Some content -> content
  in
  if not (Py.Capsule.check pyobj) then failwith "not an ocaml capsule";
  t.unwrap pyobj
;;

let unwrap t pyobj =
  try Some (unwrap_exn t pyobj) with
  | _ -> None
;;

let wrap t obj =
  let cls = Option.value_exn t.cls_object in
  Py.Object.call_function_obj_args cls [| wrap_capsule t obj |]
;;

let protect ~f =
  try f () with
  | Py.Err _ as pyerr -> raise pyerr
  | exn ->
    let msg = Printf.sprintf "ocaml error %s" (Exn.to_string_mach exn) in
    raise (Py.Err (ValueError, msg))
;;

let make ?to_string_repr ?to_string ?eq ?init name ~methods =
  let id = Id.create () in
  let t =
    let wrap, unwrap = Py.Capsule.make (Printf.sprintf "%s-%s" name (Id.to_string id)) in
    { wrap; unwrap; cls_object = None; name }
  in
  let methods =
    let to_string =
      Option.map to_string ~f:(fun fn t ~self ~args:_ ->
        fn t (fst self) |> Py.String.of_string)
    in
    let to_string_repr =
      Option.map to_string_repr ~f:(fun fn t ~self ~args:_ ->
        fn t (fst self) |> Py.String.of_string)
    in
    let to_string_repr = Option.first_some to_string_repr to_string in
    let eq =
      Option.map eq ~f:(fun fn t ~self ~args ->
        let rhs =
          match args with
          | [] -> failwith "eq with no argument"
          | _ :: _ :: _ ->
            Printf.failwithf "eq with %d arguments" (List.length args) ()
          | [ rhs ] -> rhs
        in
        fn t (fst self) (unwrap_exn t rhs) |> Py.Bool.of_bool)
    in
    List.filter_map
      [ "__str__", to_string; "__repr__", to_string_repr; "__eq__", eq ]
      ~f:(fun (name, fn) -> Option.map fn ~f:(fun fn -> Method.create name fn))
    @ methods
  in
  let methods =
    List.map methods ~f:(fun { Method.name; fn; docstring } ->
      let fn =
        let self_and_args args =
          let args = Array.to_list args in
          match args with
          | [] -> failwith "empty input"
          | p :: q -> p, q
        in
        match (fn : _ Method.fn) with
        | No_keywords fn ->
          Py.Callable.of_function ~name ?docstring (fun args ->
            protect ~f:(fun () ->
              let self, args = self_and_args args in
              fn t ~self:(unwrap_exn t self, self) ~args))
        | No_keywords_raw fn ->
          Py.Callable.of_function ~name ?docstring (fun args ->
            protect ~f:(fun () ->
              let self, args = self_and_args args in
              fn t ~self ~args))
        | With_keywords fn ->
          Py.Callable.of_function_with_keywords ~name ?docstring (fun args keywords ->
            protect ~f:(fun () ->
              let self, args = self_and_args args in
              let keywords =
                Py_module.keywords_of_python keywords |> Or_error.ok_exn
              in
              fn t ~self:(unwrap_exn t self, self) ~args ~keywords))
      in
      name, fn)
  in
  let init =
    let name = "__init__" in
    let fn =
      let docstring = Option.bind init ~f:(fun i -> i.Init.docstring) in
      Py.Callable.of_function_as_tuple ~name ?docstring (fun tuple ->
        try
          let self, args =
            match Py.Tuple.to_list tuple with
            | [] -> failwith "empty input"
            | p :: q -> p, q
          in
          let content =
            match args with
            (* Do not call the __init__ function when given a capsule as input
               as this is used when wrapping values. *)
            | [ capsule ] when Py.Capsule.check capsule -> capsule
            | _ ->
              (match init with
               | Some init -> init.fn t ~args |> wrap_capsule t
               | None -> Py.none)
          in
          Py.Object.set_attr_string self content_field content;
          Py.none
        with
        | Py.Err _ as pyerr -> raise pyerr
        | exn ->
          let msg = Printf.sprintf "ocaml error %s" (Exn.to_string_mach exn) in
          raise (Py.Err (ValueError, msg)))
    in
    name, fn
  in
  let cls_object =
    Py.Class.init name ~fields:[ content_field, Py.none ] ~methods:(init :: methods)
  in
  set_cls_object_exn t cls_object;
  t
;;

let register_in_module t modl =
  Py_module.set_value modl t.name (Option.value_exn t.cls_object)
;;

let clear_content _t pyobject = Py.Object.set_attr_string pyobject content_field Py.none
let cls_object t = Option.value_exn t.cls_object
let is_instance t pyobject = Py.Object.is_instance pyobject (cls_object t)

let set_content t pyobject v =
  Py.Object.set_attr_string pyobject content_field (wrap_capsule t v)
;;

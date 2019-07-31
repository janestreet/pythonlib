open Base
open Import

module Of_python = struct
  type 'a t =
    { type_name : string
    ; conv : pyobject -> 'a
    }
  [@@deriving fields]

  let create = Fields.create
end

module Arg = struct
  type 'a t =
    { name : string
    ; of_python : 'a Of_python.t
    ; docstring : string
    ; kind : [ `positional | `keyword of 'a option ]
    }
  [@@deriving fields]
end

module Opt_arg = struct
  type 'a t =
    { name : string
    ; of_python : 'a Of_python.t
    ; docstring : string
    }
  [@@deriving fields]
end

module T0 = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Arg : 'a Arg.t -> 'a t
    | Opt_arg : 'a Opt_arg.t -> 'a option t

  let return x = Return x
  let map t ~f = Map (t, f)
  let both t t' = Both (t, t')
  let apply f x = both f x |> map ~f:(fun (f, x) -> f x)
  let map = `Custom map
end

module T = struct
  include T0
  include Applicative.Make (T0)
end

include T

module Open_on_rhs_intf = struct
  module type S = Applicative.S
end

include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)

let valid_char c = Char.(is_alphanum c || c = '_')

let check_valid_arg_name name =
  if String.is_empty name
  then failwith "cannot use an empty name"
  else (
    let first_char = name.[0] in
    if Char.(first_char < 'a' || first_char > 'z')
    then Printf.failwithf "arg name %s does not start with a lowercase letter" name ()
    else if String.exists name ~f:(fun c -> not (valid_char c))
    then Printf.failwithf "arg name %s contains some invalid characters" name ()
    else ())
;;

let apply (type a) (t : a t) args kwargs =
  let try_of_python v ~of_python ~name =
    try Of_python.conv of_python v with
    | e ->
      value_errorf !"error processing arg %s (%s): %{Exn}" name of_python.type_name e
  in
  let kwnames = Hash_set.create (module String) in
  let positional_arguments () =
    let rec loop : type a. a t -> string list = function
      | Return _ -> []
      | Map (t, _) -> loop t
      | Both (t, t') ->
        let args = loop t in
        let args' = loop t' in
        args @ args'
      | Arg { name; kind = `positional; _ } -> [ name ]
      | Arg { kind = `keyword _; _ } -> []
      | Opt_arg _ -> []
    in
    loop t
  in
  let rec loop : type a. a t -> pos:int -> a * int =
    fun t ~pos ->
      match t with
      | Return a -> a, pos
      | Map (t, f) ->
        let v, pos = loop t ~pos in
        f v, pos
      | Both (t, t') ->
        let v, pos = loop t ~pos in
        let v', pos = loop t' ~pos in
        (v, v'), pos
      | Arg { name; of_python; docstring = _; kind = `positional } ->
        if pos >= Array.length args
        then
          value_errorf
            "not enough arguments (got %d, expected %s)"
            (Array.length args)
            (positional_arguments () |> String.concat ~sep:", ");
        try_of_python args.(pos) ~of_python ~name, pos + 1
      | Opt_arg { name; of_python; docstring = _ } ->
        if Hash_set.mem kwnames name
        then value_errorf "multiple keyword arguments with name %s" name;
        Hash_set.add kwnames name;
        let v = Map.find kwargs name in
        Option.map v ~f:(try_of_python ~of_python ~name), pos
      | Arg { name; of_python; docstring = _; kind = `keyword default } ->
        if Hash_set.mem kwnames name
        then value_errorf "multiple keyword arguments with name %s" name;
        Hash_set.add kwnames name;
        (match Map.find kwargs name with
         | Some v -> try_of_python v ~of_python ~name, pos
         | None ->
           (match default with
            | Some default -> default, pos
            | None -> value_errorf "missing keyword argument: %s" name))
  in
  let v, final_pos = loop t ~pos:0 in
  Map.iter_keys kwargs ~f:(fun key ->
    if not (Hash_set.mem kwnames key)
    then value_errorf "unexpected keyword argument %s" key);
  if final_pos <> Array.length args
  then
    value_errorf
      "expected expected %d arguments (%s), got %d"
      final_pos
      (positional_arguments () |> String.concat ~sep:", ")
      (Array.length args);
  v
;;

let params_docstring t =
  let sprintf = Printf.sprintf in
  let arg_docstring arg ~pos =
    match Arg.kind arg with
    | `positional ->
      [ sprintf
          "    :param %s: (positional %d) %s"
          (Arg.name arg)
          pos
          (Arg.docstring arg)
      ; sprintf
          "    :type %s: %s"
          (Arg.name arg)
          (Arg.of_python arg |> Of_python.type_name)
      ]
      |> String.concat ~sep:"\n"
    | `keyword default ->
      let default =
        match default with
        | None -> "mandatory keyword"
        | Some _ -> "keyword with default"
      in
      [ sprintf "    :param %s: (%s) %s" (Arg.name arg) default (Arg.docstring arg)
      ; sprintf
          "    :type %s: %s"
          (Arg.name arg)
          (Arg.of_python arg |> Of_python.type_name)
      ]
      |> String.concat ~sep:"\n"
  in
  let opt_arg_docstring arg =
    [ sprintf
        "    :param %s: (optional keyword) %s"
        (Opt_arg.name arg)
        (Opt_arg.docstring arg)
    ; sprintf
        "    :type %s: %s"
        (Opt_arg.name arg)
        (Opt_arg.of_python arg |> Of_python.type_name)
    ]
    |> String.concat ~sep:"\n"
  in
  let rec loop : type a. a t -> pos:int -> string list * int =
    fun t ~pos ->
      match t with
      | Return _ -> [], pos
      | Map (t, _) -> loop t ~pos
      | Both (t1, t2) ->
        let params1, pos = loop t1 ~pos in
        let params2, pos = loop t2 ~pos in
        params1 @ params2, pos
      | Arg ({ kind = `positional; _ } as arg) -> [ arg_docstring arg ~pos ], pos + 1
      | Arg ({ kind = `keyword _; _ } as arg) -> [ arg_docstring arg ~pos ], pos
      | Opt_arg opt_arg -> [ opt_arg_docstring opt_arg ], pos
  in
  let params, _pos = loop t ~pos:0 in
  if List.is_empty params then None else String.concat params ~sep:"\n\n" |> Option.some
;;

module Param = struct
  let positional name of_python ~docstring =
    check_valid_arg_name name;
    Arg { name; of_python; docstring; kind = `positional }
  ;;

  let keyword ?default name of_python ~docstring =
    check_valid_arg_name name;
    Arg { name; of_python; docstring; kind = `keyword default }
  ;;

  let keyword_opt name of_python ~docstring =
    check_valid_arg_name name;
    Opt_arg { name; of_python; docstring }
  ;;

  let int = Of_python.create ~type_name:"int" ~conv:int_of_python
  let float = Of_python.create ~type_name:"float" ~conv:float_of_python
  let bool = Of_python.create ~type_name:"bool" ~conv:bool_of_python
  let string = Of_python.create ~type_name:"string" ~conv:string_of_python
  let pyobject = Of_python.create ~type_name:"obj" ~conv:Fn.id

  let pair o1 o2 =
    Of_python.create
      ~type_name:
        (Printf.sprintf "(%s, %s)" (Of_python.type_name o1) (Of_python.type_name o2))
      ~conv:(fun pyobject ->
        let p1, p2 = Py.Tuple.to_tuple2 pyobject in
        Of_python.conv o1 p1, Of_python.conv o2 p2)
  ;;

  let triple o1 o2 o3 =
    Of_python.create
      ~type_name:
        (Printf.sprintf
           "(%s, %s, %s)"
           (Of_python.type_name o1)
           (Of_python.type_name o2)
           (Of_python.type_name o3))
      ~conv:(fun pyobject ->
        let p1, p2, p3 = Py.Tuple.to_tuple3 pyobject in
        Of_python.conv o1 p1, Of_python.conv o2 p2, Of_python.conv o3 p3)
  ;;

  let list o =
    Of_python.create
      ~type_name:(Printf.sprintf "[%s]" (Of_python.type_name o))
      ~conv:(Py.List.to_list_map (Of_python.conv o))
  ;;

  let one_or_tuple_or_list o =
    Of_python.create
      ~type_name:(Printf.sprintf "[%s]" (Of_python.type_name o))
      ~conv:(One_or_tuple_or_list.t_of_python (Of_python.conv o))
  ;;

  let dict ~key ~value =
    Of_python.create
      ~type_name:
        (Printf.sprintf "[%s: %s]" (Of_python.type_name key) (Of_python.type_name value))
      ~conv:(Py.Dict.to_bindings_map (Of_python.conv key) (Of_python.conv value))
  ;;
end

open Base
open Import

module Of_python = struct
  type 'a t =
    { type_name : string
    ; conv : pyobject -> 'a
    }

  let create ~type_name ~conv = { type_name; conv }
end

module Arg = struct
  type 'a t =
    { name : string
    ; of_python : 'a Of_python.t
    ; docstring : string
    ; kind : [ `positional | `keyword of 'a option ]
    }
end

module Opt_arg = struct
  type 'a t =
    { name : string
    ; of_python : 'a Of_python.t
    ; docstring : string
    }
end

module Docstring = struct
  type t = string
end

module T0 = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Arg : 'a Arg.t -> 'a t
    | Opt_arg : 'a Opt_arg.t -> 'a option t
    | Star_args : Docstring.t -> pyobject list t
    | Star_kwargs : Docstring.t -> (string, pyobject, String.comparator_witness) Map.t t

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

let no_arg fn = return () |> map ~f:fn

module State = struct
  type t =
    { pos : int
    ; after_star_args : bool
    ; after_star_kwargs : bool
    }

  let init = { pos = 0; after_star_args = false; after_star_kwargs = false }
end

let apply_ (type a) (t : a t) args kwargs =
  let try_of_python v ~of_python ~name =
    try of_python.Of_python.conv v with
    | Py.Err_with_traceback (_, msg, _) | Py.Err (_, msg) ->
      value_errorf "error processing arg %s (%s): %s" name of_python.type_name msg
    | e ->
      value_errorf
        "error processing arg %s (%s): %s"
        name
        of_python.type_name
        (Exn.to_string e)
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
      | Star_args _ -> [ "other args" ]
      | Star_kwargs _ -> []
    in
    loop t
  in
  let rec loop : type a. a t -> state:State.t -> a * State.t =
    fun t ~state ->
      match t with
      | Return a -> a, state
      | Map (t, f) ->
        let v, state = loop t ~state in
        f v, state
      | Both (t, t') ->
        let v, state = loop t ~state in
        let v', state = loop t' ~state in
        (v, v'), state
      | Arg { name; of_python; docstring = _; kind = `positional } ->
        if state.after_star_args
        then value_errorf "positional argument after *args (%s)" name;
        let pos = state.pos in
        if pos >= Array.length args
        then
          value_errorf
            "not enough arguments (got %d, expected %s)"
            (Array.length args)
            (positional_arguments () |> String.concat ~sep:", ");
        try_of_python args.(pos) ~of_python ~name, { state with pos = pos + 1 }
      | Opt_arg { name; of_python; docstring = _ } ->
        if state.after_star_kwargs
        then value_errorf "keyword argument after **kwargs (%s)" name;
        if Hash_set.mem kwnames name
        then value_errorf "multiple keyword arguments with name %s" name;
        Hash_set.add kwnames name;
        let v = Map.find kwargs name in
        Option.map v ~f:(try_of_python ~of_python ~name), state
      | Arg { name; of_python; docstring = _; kind = `keyword default } ->
        if state.after_star_kwargs
        then value_errorf "keyword argument after **kwargs (%s)" name;
        if Hash_set.mem kwnames name
        then value_errorf "multiple keyword arguments with name %s" name;
        Hash_set.add kwnames name;
        (match Map.find kwargs name with
         | Some v -> try_of_python v ~of_python ~name, state
         | None ->
           (match default with
            | Some default -> default, state
            | None -> value_errorf "missing keyword argument: %s" name))
      | Star_args _docstring ->
        if state.after_star_args then value_errorf "multiple *args";
        let total_args_len = Array.length args in
        let args =
          Array.sub args ~pos:state.pos ~len:(Array.length args - state.pos)
          |> Array.to_list
        in
        args, { state with pos = total_args_len; after_star_args = true }
      | Star_kwargs _docstring ->
        if state.after_star_kwargs then value_errorf "multiple **kwargs";
        let remaining_kwargs =
          Map.filter_keys kwargs ~f:(fun key ->
            if Hash_set.mem kwnames key
            then false
            else (
              Hash_set.add kwnames key;
              true))
        in
        remaining_kwargs, { state with after_star_kwargs = true }
  in
  let v, final_state = loop t ~state:State.init in
  Map.iter_keys kwargs ~f:(fun key ->
    if not (Hash_set.mem kwnames key)
    then value_errorf "unexpected keyword argument %s" key);
  if final_state.pos <> Array.length args
  then
    value_errorf
      "expected %d arguments (%s), got %d"
      final_state.pos
      (positional_arguments () |> String.concat ~sep:", ")
      (Array.length args);
  v
;;

let apply (type a) (t : (unit -> a) t) args kwargs =
  let f = apply_ t args kwargs in
  f ()
;;

let params_docstring t =
  let sprintf = Printf.sprintf in
  let escape_trailing_underscore s =
    (* Sphinx has an unresolved issue with trailing underscores in argument names. They
       have to be manually escaped.

       https://github.com/sphinx-doc/sphinx/issues/519
    *)
    String.chop_suffix s ~suffix:"_"
    |> Option.value_map ~default:s ~f:(fun s -> s ^ "\\_")
  in
  let arg_docstring arg ~pos =
    let arg_name = escape_trailing_underscore arg.Arg.name in
    match arg.Arg.kind with
    | `positional ->
      [ sprintf ":param %s: (positional %d) %s" arg_name pos arg.docstring
      ; sprintf ":type %s: %s" arg_name arg.of_python.type_name
      ]
      |> String.concat ~sep:"\n"
    | `keyword default ->
      let default =
        match default with
        | None -> "mandatory keyword"
        | Some _ -> "keyword with default"
      in
      [ sprintf ":param %s: (%s) %s" arg_name default arg.docstring
      ; sprintf ":type %s: %s" arg_name arg.of_python.type_name
      ]
      |> String.concat ~sep:"\n"
  in
  let opt_arg_docstring (arg : _ Opt_arg.t) =
    let arg_name = escape_trailing_underscore arg.Opt_arg.name in
    [ sprintf ":param %s: (optional keyword) %s" arg_name arg.docstring
    ; sprintf ":type %s: %s" arg_name arg.of_python.type_name
    ]
    |> String.concat ~sep:"\n"
  in
  let star_args_docstring doc = sprintf ":param other args: %s" doc in
  let star_kwargs_docstring doc = sprintf ":param other keyword args: %s" doc in
  let rec loop : type a. a t -> pos:int -> _ list * int =
    fun t ~pos ->
      match t with
      | Return _ -> [], pos
      | Map (t, _) -> loop t ~pos
      | Both (t1, t2) ->
        let params1, pos = loop t1 ~pos in
        let params2, pos = loop t2 ~pos in
        params1 @ params2, pos
      | Arg ({ kind = `positional; _ } as arg) -> [ `pos (arg_docstring arg ~pos) ], pos + 1
      | Arg ({ kind = `keyword None; _ } as arg) ->
        [ `kw_mandatory (arg_docstring arg ~pos) ], pos
      | Arg ({ kind = `keyword (Some _); _ } as arg) ->
        [ `kw_opt (arg_docstring arg ~pos) ], pos
      | Opt_arg opt_arg -> [ `kw_opt (opt_arg_docstring opt_arg) ], pos
      | Star_args doc ->
        (* There should be no other positional arg past this one *)
        [ `other (star_args_docstring doc) ], Int.max_value_30_bits
      | Star_kwargs doc -> [ `other (star_kwargs_docstring doc) ], pos
  in
  let params, _pos = loop t ~pos:0 in
  let params =
    List.stable_sort params ~compare:(fun param1 param2 ->
      (* Positional parameters are first, then mandatory keywords, then optional keywords. *)
      match param1, param2 with
      | `pos _, `kw_mandatory _ -> -1
      | `kw_mandatory _, `pos _ -> 1
      | `pos _, `kw_opt _ -> -1
      | `kw_opt _, `pos _ -> 1
      | `kw_mandatory _, `kw_opt _ -> -1
      | `kw_opt _, `kw_mandatory _ -> 1
      | _ -> 0)
    |> List.map ~f:(function `pos str | `kw_mandatory str | `kw_opt str | `other str ->
      str)
  in
  if List.is_empty params then None else String.concat params ~sep:"\n\n" |> Option.some
;;

let params_docstring ?docstring t =
  [ params_docstring t; docstring ]
  |> List.filter_opt
  |> String.concat ~sep:"\n\n"
  |> Printf.sprintf "\n%s"
;;

module Param = struct
  let choice (o1 : _ Of_python.t) (o2 : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "%s | %s" o1.type_name o2.type_name)
      ~conv:(fun pyobject ->
        try Either.First (o1.conv pyobject) with
        | _ -> Second (o2.conv pyobject))
  ;;

  let map (o : _ Of_python.t) ~f =
    Of_python.create ~type_name:o.type_name ~conv:(fun pyobject -> o.conv pyobject |> f)
  ;;

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

  let path =
    Of_python.create ~type_name:"path" ~conv:(fun pyobject ->
      try string_of_python pyobject with
      | _ ->
        if Py.Object.is_instance pyobject (Lazy.force path_cls)
        then (
          let str = get_from_builtins "str" in
          Py.Object.call_function_obj_args str [| pyobject |] |> string_of_python)
        else
          value_errorf
            "expected a str or instance of Path, got %s"
            (Py.Type.get pyobject |> Py.Type.name)
            ())
  ;;

  (* Rather than using typerep, it would be nice to have a [function] combinator
     and let users write e.g [function (pair int int) int] for addition.

     However when calling a (python) closure from ocaml with some parameters
     these parameters will have to be converted from ocaml to python.
     Currently [params] only handles the conversion from python to ocaml which is
     problematic.
  *)
  let typerep tr =
    Of_python.create
      ~type_name:(Py_typerep.to_ocaml tr)
      ~conv:(Py_typerep.python_to_ocaml tr)
  ;;

  let pyobject = Of_python.create ~type_name:"obj" ~conv:Fn.id

  let check_tuple_len pyobject ~expected_length =
    if not (Py.Tuple.check pyobject)
    then
      Printf.failwithf "expected a tuple got %s" (Py.Type.get pyobject |> Py.Type.name) ();
    let length = Py.Tuple.size pyobject in
    if expected_length <> length
    then
      Printf.failwithf
        "expected a tuple with %d elements, got %d"
        expected_length
        length
        ()
  ;;

  let pair (o1 : _ Of_python.t) (o2 : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "Tuple[%s, %s]" o1.type_name o2.type_name)
      ~conv:(fun pyobject ->
        check_tuple_len pyobject ~expected_length:2;
        let p1, p2 = Py.Tuple.to_tuple2 pyobject in
        o1.conv p1, o2.conv p2)
  ;;

  let triple (o1 : _ Of_python.t) (o2 : _ Of_python.t) (o3 : _ Of_python.t) =
    Of_python.create
      ~type_name:
        (Printf.sprintf "Tuple[%s, %s, %s]" o1.type_name o2.type_name o3.type_name)
      ~conv:(fun pyobject ->
        check_tuple_len pyobject ~expected_length:3;
        let p1, p2, p3 = Py.Tuple.to_tuple3 pyobject in
        o1.conv p1, o2.conv p2, o3.conv p3)
  ;;

  let quadruple
        (o1 : _ Of_python.t)
        (o2 : _ Of_python.t)
        (o3 : _ Of_python.t)
        (o4 : _ Of_python.t)
    =
    Of_python.create
      ~type_name:
        (Printf.sprintf
           "Tuple[%s, %s, %s, %s]"
           o1.type_name
           o2.type_name
           o3.type_name
           o4.type_name)
      ~conv:(fun pyobject ->
        check_tuple_len pyobject ~expected_length:3;
        let p1, p2, p3, p4 = Py.Tuple.to_tuple4 pyobject in
        o1.conv p1, o2.conv p2, o3.conv p3, o4.conv p4)
  ;;

  let quintuple
        (o1 : _ Of_python.t)
        (o2 : _ Of_python.t)
        (o3 : _ Of_python.t)
        (o4 : _ Of_python.t)
        (o5 : _ Of_python.t)
    =
    Of_python.create
      ~type_name:
        (Printf.sprintf
           "Tuple[%s, %s, %s, %s, %s]"
           o1.type_name
           o2.type_name
           o3.type_name
           o4.type_name
           o5.type_name)
      ~conv:(fun pyobject ->
        check_tuple_len pyobject ~expected_length:3;
        let p1, p2, p3, p4, p5 = Py.Tuple.to_tuple5 pyobject in
        o1.conv p1, o2.conv p2, o3.conv p3, o4.conv p4, o5.conv p5)
  ;;

  let option (o : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "Optional[%s]" o.type_name)
      ~conv:(fun python_value ->
        if Py.is_null python_value || Py.is_none python_value
        then None
        else Some (o.conv python_value))
  ;;

  let list (o : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "List[%s]" o.type_name)
      ~conv:(fun python_value ->
        (match Py.Type.get python_value with
         | List | Tuple -> ()
         | otherwise ->
           Printf.failwithf "not a list or a tuple (%s)" (Py.Type.name otherwise) ());
        py_list_to_list_map_safe o.conv python_value)
  ;;

  let list_or_iter (o : _ Of_python.t) =
    Of_python.create ~type_name:(Printf.sprintf "List[%s]" o.type_name) ~conv:(fun p ->
      match iterable_to_list p with
      | None ->
        Printf.failwithf "not a list/tuple/iter (%s)" (Py.Type.get p |> Py.Type.name) ()
      | Some l -> py_list_to_list_map_safe o.conv l)
  ;;

  let array_or_iter (o : _ Of_python.t) =
    Of_python.create ~type_name:(Printf.sprintf "List[%s]" o.type_name) ~conv:(fun p ->
      match iterable_to_list p with
      | None ->
        Printf.failwithf "not a list/tuple/iter (%s)" (Py.Type.get p |> Py.Type.name) ()
      | Some l -> py_list_to_array_map_safe o.conv l)
  ;;

  let one_or_tuple_or_list (o : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "ListOrSingleElt[%s]" o.type_name)
      ~conv:(One_or_tuple_or_list.t_of_python o.conv)
  ;;

  let one_or_tuple_or_list_relaxed (o : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "ListOrSingleElt[%s] (relaxed)" o.type_name)
      ~conv:(One_or_tuple_or_list_or_error.t_of_python o.conv ~type_name:o.type_name)
  ;;

  let with_broadcast (o : _ Of_python.t) ~arg_name =
    Of_python.create
      ~type_name:(Printf.sprintf "ListWithBroadcast[%s]" o.type_name)
      ~conv:(fun pyobject -> Broadcast.create pyobject o.conv ~arg_name)
  ;;

  let positional_broadcast arg_name of_python =
    positional arg_name (with_broadcast of_python ~arg_name)
  ;;

  let keyword_broadcast ?default arg_name of_python =
    keyword ?default arg_name (with_broadcast of_python ~arg_name)
  ;;

  let keyword_opt_broadcast arg_name of_python =
    keyword_opt arg_name (with_broadcast of_python ~arg_name)
  ;;

  let dict ~(key : _ Of_python.t) ~(value : _ Of_python.t) =
    Of_python.create
      ~type_name:(Printf.sprintf "Dict[%s, %s]" key.type_name value.type_name)
      ~conv:(Py.Dict.to_bindings_map key.conv value.conv)
  ;;

  let star_args ~docstring = Star_args docstring
  let star_kwargs ~docstring = Star_kwargs docstring

  let kind_to_string : type a b. (a, b) Bigarray.kind -> string = function
    | Float32 -> "np.float32"
    | Float64 -> "np.float64"
    | Int8_unsigned -> "np.uint8"
    | Int8_signed -> "np.int8"
    | Int16_unsigned -> "np.uint16"
    | Int16_signed -> "np.int16"
    | Int32 -> "np.int32"
    | Int64 -> "np.int64"
    | Int -> "np.int"
    | Char -> "np.byte"
    | Nativeint -> "np.int"
    | Complex32 -> "np.complex32"
    | Complex64 -> "np.complex64"
  ;;

  let layout_to_string : type c. c Bigarray.layout -> string = function
    | C_layout -> "C"
    | Fortran_layout -> "F"
  ;;

  let to_numpy_array ?dims kind layout p =
    if not (Py.Object.is_instance p (Py.Array.pyarray_type ()))
    then value_errorf "expected a numpy array, got %s" (Py.Type.get p |> Py.Type.name);
    let bigarray = Numpy.to_bigarray kind layout p in
    Option.iter dims ~f:(fun dims ->
      let num_dims = Bigarray.Genarray.num_dims bigarray in
      if dims <> num_dims
      then value_errorf "expected a numpy array with %d dims, got %d" dims num_dims);
    bigarray
  ;;

  let numpy_type_name ?dims kind layout =
    Printf.sprintf
      "np.array(dtype=%s, order='%s'%s)"
      (kind_to_string kind)
      (layout_to_string layout)
      (Option.value_map dims ~default:"" ~f:(Printf.sprintf ", dims=%d"))
  ;;

  let numpy_array kind layout =
    Of_python.create
      ~type_name:(numpy_type_name kind layout)
      ~conv:(to_numpy_array kind layout)
  ;;

  let numpy_array1 kind layout =
    Of_python.create ~type_name:(numpy_type_name ~dims:1 kind layout) ~conv:(fun p ->
      to_numpy_array ~dims:1 kind layout p |> Bigarray.array1_of_genarray)
  ;;

  let numpy_array2 kind layout =
    Of_python.create ~type_name:(numpy_type_name ~dims:2 kind layout) ~conv:(fun p ->
      to_numpy_array ~dims:2 kind layout p |> Bigarray.array2_of_genarray)
  ;;

  let numpy_array3 kind layout =
    Of_python.create ~type_name:(numpy_type_name ~dims:3 kind layout) ~conv:(fun p ->
      to_numpy_array ~dims:3 kind layout p |> Bigarray.array3_of_genarray)
  ;;
end

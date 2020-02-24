module Typerep = Typerep_lib.Std.Typerep
open Base
open Import

(* Register some named types so that supported types can easily be extended. *)
module Named_types : sig
  val register_exn
    :  name:string
    -> ocaml_type:string
    -> python_to_ocaml:(pyobject -> 'a)
    -> ocaml_to_python:('a -> pyobject)
    -> unit

  val mem : string -> bool
  val find_ocaml_type : name:string -> string option
  val typerep_exn : string -> Typerep.packed
  val ocaml_to_python : 'a Typerep.t -> 'a -> name:string -> pyobject
  val python_to_ocaml : 'a Typerep.t -> pyobject -> name:string -> 'a
  val ocaml_type : name:string -> string
end = struct
  type 'a t =
    { typename : 'a Typerep_lib.Typename.typename
    ; typerep : 'a Typerep.t
    ; ocaml_type : string
    ; python_to_ocaml : pyobject -> 'a
    ; ocaml_to_python : 'a -> pyobject
    }

  type packed = T : 'a t -> packed

  let store = Hashtbl.create (module String)

  let register_exn (type a) ~name ~ocaml_type ~python_to_ocaml ~ocaml_to_python =
    let typename = Typerep_lib.Typename.create ~name () in
    let typerep =
      let module T0 = struct
        type named = a
        type t = a

        let typename_of_named = typename
        let typename_of_t = typename
        let witness = Type_equal.T
      end
      in
      Typerep.Named (Typerep.Named.T0 (module T0), None)
    in
    let t = { typename; typerep; ocaml_type; python_to_ocaml; ocaml_to_python } in
    Hashtbl.add_exn store ~key:name ~data:(T t)
  ;;

  let mem = Hashtbl.mem store

  let find_ocaml_type ~name =
    Option.map (Hashtbl.find store name) ~f:(fun (T t) -> t.ocaml_type)
  ;;

  let find_exn ~name =
    match Hashtbl.find store name with
    | None -> Printf.failwithf "not a registered name type: %s" name ()
    | Some packed -> packed
  ;;

  let typerep_exn s =
    let (T t) = Hashtbl.find_exn store s in
    Typerep.T t.typerep
  ;;

  let ocaml_type ~name =
    let (T t) = find_exn ~name in
    t.ocaml_type
  ;;

  let ocaml_to_python typerep o ~name =
    let (T t) = find_exn ~name in
    match Typerep.same_witness typerep t.typerep with
    | None -> Printf.failwithf "type witness mismatch: %s" name ()
    | Some equal ->
      let o = Type_equal.conv equal o in
      t.ocaml_to_python o
  ;;

  let python_to_ocaml typerep pyobj ~name =
    let (T t) = find_exn ~name in
    match Typerep.same_witness t.typerep typerep with
    | None -> Printf.failwithf "type witness mismatch: %s" name ()
    | Some equal -> t.python_to_ocaml pyobj |> Type_equal.conv equal
  ;;
end

let rec to_ocaml : type a. a Typerep.t -> string = function
  | Unit -> "unit"
  | Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Bool -> "bool"
  | Option t -> Printf.sprintf "(%s) option" (to_ocaml t)
  | List t -> Printf.sprintf "(%s) list" (to_ocaml t)
  | Array t -> Printf.sprintf "(%s) array" (to_ocaml t)
  | Tuple (T2 (t1, t2)) -> tuple_to_ocaml [ to_ocaml t1; to_ocaml t2 ]
  | Tuple (T3 (t1, t2, t3)) -> tuple_to_ocaml [ to_ocaml t1; to_ocaml t2; to_ocaml t3 ]
  | Tuple (T4 (t1, t2, t3, t4)) ->
    tuple_to_ocaml [ to_ocaml t1; to_ocaml t2; to_ocaml t3; to_ocaml t4 ]
  | Tuple (T5 (t1, t2, t3, t4, t5)) ->
    tuple_to_ocaml [ to_ocaml t1; to_ocaml t2; to_ocaml t3; to_ocaml t4; to_ocaml t5 ]
  | Int32 -> failwith "not supported"
  | Int64 -> failwith "not supported"
  | Nativeint -> failwith "not supported"
  | Char -> failwith "not supported"
  | Bytes -> failwith "not supported"
  | Lazy _ -> failwith "not supported"
  | Ref _ -> failwith "not supported"
  | Function (t1, t2) -> Printf.sprintf "%s -> %s" (to_ocaml t1) (to_ocaml t2)
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.ocaml_type ~name

and tuple_to_ocaml strs =
  List.map strs ~f:(fun s -> Printf.sprintf "(%s)" s) |> String.concat ~sep:" * "
;;

let check o ~name ~check =
  if not (check o)
  then Printf.failwithf "expected %s, got %s" name (Py.Type.get o |> Py.Type.name) ()
;;

let check_tuple pyobject ~n =
  check pyobject ~name:"tuple" ~check:Py.Tuple.check;
  let size = Py.Tuple.size pyobject in
  if size <> n then Printf.failwithf "expected a tuple of size %d, got %d" n size ()
;;

let protect ~f x =
  try f x with
  | Py.Err _ as err -> raise err
  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))
;;

let rec ocaml_to_python : type a. a Typerep.t -> a -> pyobject =
  fun t o ->
  match t with
  | Unit -> Py.none
  | Int -> Py.Int.of_int o
  | String -> Py.String.of_string o
  | Float -> Py.Float.of_float o
  | Bool -> Py.Bool.of_bool o
  | Option t ->
    (match o with
     | None -> Py.none
     | Some o -> ocaml_to_python t o)
  | List t -> Py.List.of_list_map (ocaml_to_python t) o
  | Array t -> Py.List.of_array_map (ocaml_to_python t) o
  | Tuple (T2 (t1, t2)) ->
    let o1, o2 = o in
    Py.Tuple.of_tuple2 (ocaml_to_python t1 o1, ocaml_to_python t2 o2)
  | Tuple (T3 (t1, t2, t3)) ->
    let o1, o2, o3 = o in
    Py.Tuple.of_tuple3
      (ocaml_to_python t1 o1, ocaml_to_python t2 o2, ocaml_to_python t3 o3)
  | Tuple (T4 (t1, t2, t3, t4)) ->
    let o1, o2, o3, o4 = o in
    Py.Tuple.of_tuple4
      ( ocaml_to_python t1 o1
      , ocaml_to_python t2 o2
      , ocaml_to_python t3 o3
      , ocaml_to_python t4 o4 )
  | Tuple (T5 (t1, t2, t3, t4, t5)) ->
    let o1, o2, o3, o4, o5 = o in
    Py.Tuple.of_tuple5
      ( ocaml_to_python t1 o1
      , ocaml_to_python t2 o2
      , ocaml_to_python t3 o3
      , ocaml_to_python t4 o4
      , ocaml_to_python t5 o5 )
  | Int32 -> failwith "not supported"
  | Int64 -> failwith "not supported"
  | Nativeint -> failwith "not supported"
  | Char -> failwith "not supported"
  | Bytes -> failwith "not supported"
  | Lazy _ -> failwith "not supported"
  | Ref _ -> failwith "not supported"
  | Function (t1, t2) ->
    Py.Callable.of_function
      ~docstring:(to_ocaml t)
      (protect ~f:(fun pyobjects ->
         let pyobject =
           match pyobjects with
           | [||] -> Py.none
           | [| pyobject |] -> pyobject
           | array -> Py.Tuple.of_array array
         in
         python_to_ocaml t1 pyobject |> o |> ocaml_to_python t2))
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.ocaml_to_python t o ~name

and python_to_ocaml : type a. a Typerep.t -> pyobject -> a =
  fun t pyobj ->
  match t with
  | Unit -> check pyobj ~name:"none" ~check:Py.is_none
  | Int ->
    (* We cannot use Py.Int.check as it actually checks for the type
       being Long rather than Int. *)
    check pyobj ~name:"int" ~check:(fun o ->
      match Py.Type.get o with
      | Int | Long -> true
      | _ -> false);
    Py.Int.to_int pyobj
  | String ->
    check pyobj ~name:"string" ~check:Py.String.check;
    Py.String.to_string pyobj
  | Float ->
    check pyobj ~name:"float" ~check:(fun o ->
      match Py.Type.get o with
      | Int | Long | Float -> true
      | _ -> false);
    Py.Float.to_float pyobj
  | Bool ->
    check pyobj ~name:"bool" ~check:Py.Bool.check;
    Py.Bool.to_bool pyobj
  | Option t ->
    (match Py.Type.get pyobj with
     | None | Null -> None
     | _ -> Some (python_to_ocaml t pyobj))
  | List t ->
    check pyobj ~name:"list" ~check:Py.List.check;
    Py.List.to_list_map (python_to_ocaml t) pyobj
  | Array t ->
    check pyobj ~name:"list" ~check:Py.List.check;
    Py.List.to_array_map (python_to_ocaml t) pyobj
  | Tuple (T2 (t1, t2)) ->
    (* This check avoids a segfault as getting items from non-tuples return null.

       (The reason we check in the other case is for better error messages and possible
       future changes.)  *)
    check_tuple pyobj ~n:2;
    let p1, p2 = Py.Tuple.to_tuple2 pyobj in
    python_to_ocaml t1 p1, python_to_ocaml t2 p2
  | Tuple (T3 (t1, t2, t3)) ->
    check_tuple pyobj ~n:3;
    let p1, p2, p3 = Py.Tuple.to_tuple3 pyobj in
    python_to_ocaml t1 p1, python_to_ocaml t2 p2, python_to_ocaml t3 p3
  | Tuple (T4 (t1, t2, t3, t4)) ->
    check_tuple pyobj ~n:4;
    let p1, p2, p3, p4 = Py.Tuple.to_tuple4 pyobj in
    ( python_to_ocaml t1 p1
    , python_to_ocaml t2 p2
    , python_to_ocaml t3 p3
    , python_to_ocaml t4 p4 )
  | Tuple (T5 (t1, t2, t3, t4, t5)) ->
    check_tuple pyobj ~n:5;
    let p1, p2, p3, p4, p5 = Py.Tuple.to_tuple5 pyobj in
    ( python_to_ocaml t1 p1
    , python_to_ocaml t2 p2
    , python_to_ocaml t3 p3
    , python_to_ocaml t4 p4
    , python_to_ocaml t5 p5 )
  | Int32 -> failwith "not supported"
  | Int64 -> failwith "not supported"
  | Nativeint -> failwith "not supported"
  | Char -> failwith "not supported"
  | Bytes -> failwith "not supported"
  | Lazy _ -> failwith "not supported"
  | Ref _ -> failwith "not supported"
  | Function ((Tuple _ as t1), t2) ->
    check pyobj ~name:"callable" ~check:Py.Callable.check;
    protect ~f:(fun x ->
      ocaml_to_python t1 x
      |> Py.Callable.to_function_as_tuple pyobj
      |> python_to_ocaml t2)
  | Function (t1, t2) ->
    check pyobj ~name:"callable" ~check:Py.Callable.check;
    protect ~f:(fun x ->
      [| ocaml_to_python t1 x |] |> Py.Callable.to_function pyobj |> python_to_ocaml t2)
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.python_to_ocaml t pyobj ~name
;;

let%expect_test "obj" =
  if not (Py.is_initialized ()) then Py.initialize ~version:3 ();
  let print_list elt_to_string l =
    List.map l ~f:elt_to_string |> String.concat ~sep:" " |> Stdio.printf "%s\n%!"
  in
  let roundtrip : type a. a Typerep.t -> a -> a =
    fun t v -> ocaml_to_python t v |> python_to_ocaml t
  in
  let () = roundtrip Unit () in
  print_list Int.to_string (List.map [ -1; 0; 42 ] ~f:(roundtrip Int));
  [%expect {| -1 0 42 |}];
  print_list Float.to_string (List.map [ -2.71828; 3.1415 ] ~f:(roundtrip Float));
  [%expect {| -2.71828 3.1415 |}];
  print_list Float.to_string (roundtrip (List Float) [ -2.71828; 3.1415 ]);
  [%expect {| -2.71828 3.1415 |}];
  print_list
    (fun (f, i, o) ->
       let o =
         Option.value_map o ~default:"()" ~f:(fun (s, b) -> Printf.sprintf "%s %b" s b)
       in
       Printf.sprintf "(%f %i %s)" f i o)
    (roundtrip
       (List (Tuple (T3 (Float, Int, Option (Tuple (T2 (String, Bool)))))))
       [ 3.14, 42, None; 2.71828, 1337, Some ("test", true) ]);
  [%expect {| (3.140000 42 ()) (2.718280 1337 test true) |}]
;;

let register_named_type ~name ~ocaml_type =
  match Named_types.find_ocaml_type ~name with
  | Some otype when String.( = ) ocaml_type otype ->
    (* In this case we consider the result a no-op. *)
    ()
  | Some otype ->
    Printf.failwithf "Type %s already exists and is bound to %s." name otype ()
  | None ->
    let ocaml_to_python, python_to_ocaml =
      Py.Capsule.make (Printf.sprintf "%s-%s" name ocaml_type)
    in
    Named_types.register_exn ~name ~ocaml_type ~python_to_ocaml ~ocaml_to_python
;;

let rec of_type : Type.t -> Typerep.packed = function
  | Atom (_, "unit") -> T Unit
  | Atom (_, "int") -> T Int
  | Atom (_, "float") -> T Float
  | Atom (_, "bool") -> T Bool
  | Atom (_, "string") -> T String
  | Atom (_, str) when Named_types.mem str -> Named_types.typerep_exn str
  | Atom (_, str) -> Printf.failwithf "unknown type %s" str ()
  | Tuple2 (t1, t2) ->
    let (T t1) = of_type t1 in
    let (T t2) = of_type t2 in
    T (Tuple (T2 (t1, t2)))
  | Tuple3 (t1, t2, t3) ->
    let (T t1) = of_type t1 in
    let (T t2) = of_type t2 in
    let (T t3) = of_type t3 in
    T (Tuple (T3 (t1, t2, t3)))
  | Tuple4 (t1, t2, t3, t4) ->
    let (T t1) = of_type t1 in
    let (T t2) = of_type t2 in
    let (T t3) = of_type t3 in
    let (T t4) = of_type t4 in
    T (Tuple (T4 (t1, t2, t3, t4)))
  | Tuple5 (t1, t2, t3, t4, t5) ->
    let (T t1) = of_type t1 in
    let (T t2) = of_type t2 in
    let (T t3) = of_type t3 in
    let (T t4) = of_type t4 in
    let (T t5) = of_type t5 in
    T (Tuple (T5 (t1, t2, t3, t4, t5)))
  | Arrow (_, t1, t2) ->
    let (T t1) = of_type t1 in
    let (T t2) = of_type t2 in
    T (Function (t1, t2))
  | Apply (t, "list") ->
    let (T t) = of_type t in
    T (List t)
  | Apply (t, "array") ->
    let (T t) = of_type t in
    T (Array t)
  | Apply (t, "option") ->
    let (T t) = of_type t in
    T (Option t)
  | Apply (_t, str) -> Printf.failwithf "unknown type %s" str ()
;;

let parse str =
  Type_parser.type_expr Type_lexer.token (Lexing.from_string str) |> of_type
;;

let%expect_test "parse-type" =
  if not (Py.is_initialized ()) then Py.initialize ();
  List.iteri
    [ "unit"
    ; "  int"
    ; "unit -> int"
    ; "string list -> float * bool option"
    ; "int * (string * int * bool)"
    ; "(int*int*int*(int*string list * int) list) option -> int * (string * int * bool)"
    ; "(int*int*int array*(int*string array * int) list) option -> string array"
    ; "int array list option array -> string array array"
    ; "(((int array list) option)) array -> (((string array)) array)"
    ]
    ~f:(fun index str ->
      let (T t) = parse str in
      Stdio.printf "%d %s\n%!" index (to_ocaml t));
  [%expect
    {|
        0 unit
        1 int
        2 unit -> int
        3 (string) list -> (float) * ((bool) option)
        4 (int) * ((string) * (int) * (bool))
        5 ((int) * (int) * (int) * (((int) * ((string) list) * (int)) list)) option -> (int) * ((string) * (int) * (bool))
        6 ((int) * (int) * ((int) array) * (((int) * ((string) array) * (int)) list)) option -> (string) array
        7 ((((int) array) list) option) array -> ((string) array) array
        8 ((((int) array) list) option) array -> ((string) array) array |}]
;;

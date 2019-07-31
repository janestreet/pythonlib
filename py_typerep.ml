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
  [@@deriving fields]

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
    Option.map (Hashtbl.find store name) ~f:(fun (T t) -> ocaml_type t)
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
  | Function _ -> failwith "not supported"
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.ocaml_type ~name

and tuple_to_ocaml strs =
  List.map strs ~f:(fun s -> Printf.sprintf "(%s)" s) |> String.concat ~sep:" * "
;;

(* Caution: this assumes that [obj] has the type represented by [t]. *)
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
  | Function _ -> failwith "not supported"
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.ocaml_to_python t o ~name
;;

let rec python_to_ocaml : type a. a Typerep.t -> pyobject -> a =
  fun t pyobj ->
  match t with
  | Unit -> ()
  | Int -> Py.Int.to_int pyobj
  | String -> Py.String.to_string pyobj
  | Float -> Py.Float.to_float pyobj
  | Bool -> Py.Bool.to_bool pyobj
  | Option t ->
    (match Py.Type.get pyobj with
     | None | Null -> None
     | _ -> Some (python_to_ocaml t pyobj))
  | List t -> Py.List.to_list_map (python_to_ocaml t) pyobj
  | Array t -> Py.List.to_array_map (python_to_ocaml t) pyobj
  | Tuple (T2 (t1, t2)) ->
    let p1, p2 = Py.Tuple.to_tuple2 pyobj in
    python_to_ocaml t1 p1, python_to_ocaml t2 p2
  | Tuple (T3 (t1, t2, t3)) ->
    let p1, p2, p3 = Py.Tuple.to_tuple3 pyobj in
    python_to_ocaml t1 p1, python_to_ocaml t2 p2, python_to_ocaml t3 p3
  | Tuple (T4 (t1, t2, t3, t4)) ->
    let p1, p2, p3, p4 = Py.Tuple.to_tuple4 pyobj in
    ( python_to_ocaml t1 p1
    , python_to_ocaml t2 p2
    , python_to_ocaml t3 p3
    , python_to_ocaml t4 p4 )
  | Tuple (T5 (t1, t2, t3, t4, t5)) ->
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
  | Function _ -> failwith "not supported"
  | Record _ -> failwith "not supported"
  | Variant _ -> failwith "not supported"
  | Named (named, _) ->
    let name = Typerep.Named.typename_of_t named |> Typerep_lib.Typename.name in
    Named_types.python_to_ocaml t pyobj ~name
;;

let%expect_test "obj" =
  if not (Py.is_initialized ()) then Py.initialize ();
  let roundtrip : type a. a Typerep.t -> a -> a =
    fun t v -> ocaml_to_python t v |> python_to_ocaml t
  in
  let () = roundtrip Unit () in
  Stdio.printf !"%{sexp:int list}\n%!" (List.map [ -1; 0; 42 ] ~f:(roundtrip Int));
  [%expect {| (-1 0 42) |}];
  Stdio.printf
    !"%{sexp:float list}\n%!"
    (List.map [ -2.71828; 3.1415 ] ~f:(roundtrip Float));
  [%expect {| (-2.71828 3.1415) |}];
  Stdio.printf !"%{sexp:float list}\n%!" (roundtrip (List Float) [ -2.71828; 3.1415 ]);
  [%expect {| (-2.71828 3.1415) |}];
  Stdio.printf
    !"%{sexp:(float * int * (string * bool)option) list}\n%!"
    (roundtrip
       (List (Tuple (T3 (Float, Int, Option (Tuple (T2 (String, Bool)))))))
       [ 3.14, 42, None; 2.71828, 1337, Some ("test", true) ]);
  [%expect {| ((3.14 42 ()) (2.71828 1337 ((test true)))) |}]
;;

(* This is used by the parser below, it's similar to [String.split] except that no
   split is performed on characters that are within parenthesis blocks.
*)
let split_on_unescaped str ~on =
  let par = ref 0 in
  String.to_list str
  |> List.groupi ~break:(fun _ c1 _ ->
    match c1 with
    | '(' ->
      Int.incr par;
      false
    | ')' ->
      Int.decr par;
      false
    | _ -> Char.( = ) c1 on && !par = 0)
  |> List.filter_map ~f:(fun cs ->
    let cs =
      String.of_char_list cs
      |> String.strip ~drop:(fun c -> Char.is_whitespace c || Char.( = ) c on)
    in
    if String.is_empty cs then None else Some cs)
;;

let%expect_test "split" =
  if not (Py.is_initialized ()) then Py.initialize ();
  List.iteri
    [ "a"; "a*bc"; "a*bc*d*()*a"; "a*(b*(c))*d"; "((a)*b)*c" ]
    ~f:(fun index str ->
      let strs = split_on_unescaped str ~on:'*' in
      Stdio.printf !"%d %{sexp:string list}\n%!" index strs);
  [%expect
    {|
        0 (a)
        1 (a bc)
        2 (a bc d "()" a)
        3 (a "(b*(c))" d)
        4 ("((a)*b)" c) |}]
;;

(* Hacky type parser. *)
let parse str =
  let rec parse_expr : string -> Typerep.packed =
    fun str ->
      let str = String.strip str in
      if Char.( = ) str.[0] '(' && Char.( = ) str.[String.length str - 1] ')'
      then String.sub str ~pos:1 ~len:(String.length str - 2) |> parse_expr
      else (
        match str with
        | "unit" -> T Unit
        | "int" -> T Int
        | "float" -> T Float
        | "bool" -> T Bool
        | "string" -> T String
        | str when Named_types.mem str -> Named_types.typerep_exn str
        | str ->
          (match split_on_unescaped str ~on:'*' with
           | [] -> failwith "empty type"
           | [ s1 ] ->
             (match split_on_unescaped s1 ~on:' ' with
              | [] -> failwith "empty type"
              | [ p ] -> Printf.failwithf "unknown type %s" p ()
              | p :: q ->
                List.fold q ~init:(parse_expr p) ~f:(fun (T acc) ->
                  function
                  | "list" -> T (List acc)
                  | "array" -> T (Array acc)
                  | "option" -> T (Option acc)
                  | otherwise -> Printf.failwithf "not a type constructor %s" otherwise ()))
           | [ s1; s2 ] ->
             let (T t1) = parse_expr s1 in
             let (T t2) = parse_expr s2 in
             T (Tuple (T2 (t1, t2)))
           | [ s1; s2; s3 ] ->
             let (T t1) = parse_expr s1 in
             let (T t2) = parse_expr s2 in
             let (T t3) = parse_expr s3 in
             T (Tuple (T3 (t1, t2, t3)))
           | [ s1; s2; s3; s4 ] ->
             let (T t1) = parse_expr s1 in
             let (T t2) = parse_expr s2 in
             let (T t3) = parse_expr s3 in
             let (T t4) = parse_expr s4 in
             T (Tuple (T4 (t1, t2, t3, t4)))
           | [ s1; s2; s3; s4; s5 ] ->
             let (T t1) = parse_expr s1 in
             let (T t2) = parse_expr s2 in
             let (T t3) = parse_expr s3 in
             let (T t4) = parse_expr s4 in
             let (T t5) = parse_expr s5 in
             T (Tuple (T5 (t1, t2, t3, t4, t5)))
           | tuple ->
             Printf.failwithf
               "tuples of length greater than 5 (%d) are not supported"
               (List.length tuple)
               ()))
  in
  parse_expr str
;;

let parse_maybe_fn str =
  match String.substr_index_all str ~may_overlap:false ~pattern:"->" with
  | [] -> `value (parse str)
  | [ index ] ->
    let lhs = String.sub str ~pos:0 ~len:index |> parse in
    let rhs =
      String.sub str ~pos:(index + 2) ~len:(String.length str - index - 2) |> parse
    in
    `fn (lhs, rhs)
  | _ :: _ :: _ -> Printf.failwithf "unable to parse type %s" str ()
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
      Py.Capsule.make (Printf.sprintf !"%s-%s" name ocaml_type)
    in
    Named_types.register_exn ~name ~ocaml_type ~python_to_ocaml ~ocaml_to_python
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
      let str =
        match parse_maybe_fn str with
        | `fn (T t1, T t2) -> Printf.sprintf "%s -> %s" (to_ocaml t1) (to_ocaml t2)
        | `value (T t) -> to_ocaml t
      in
      Stdio.printf !"%d %s\n%!" index str);
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

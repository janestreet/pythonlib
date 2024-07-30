open Base
open Python_lib
open Python_lib.Let_syntax

type t =
  { foo : string
  ; bar : (int * float) list
  }
[@@deriving python]

let add =
  let%map_open arg1 = positional_only "lhs" int ~docstring:""
  and arg2 = positional_only "rhs" int ~docstring:"" in
  fun () -> python_of_int (arg1 + arg2)
;;

let make_t =
  let%map_open foo = positional_only "foo" string ~docstring:""
  and repeats = keyword "repeats" int ~docstring:""
  and bar1 = keyword "bar1" int ~default:42 ~docstring:""
  and bar2 = keyword "bar2" float ~default:3.14 ~docstring:"" in
  fun () -> python_of_t { foo; bar = List.init repeats ~f:(fun i -> bar1 + i, bar2) }
;;

let cartesian_product =
  let%map_open l1 = positional_only "l1" (list pyobject) ~docstring:""
  and l2 = positional_only "l2" (list pyobject) ~docstring:"" in
  fun () -> List.cartesian_product l1 l2 |> python_of_list Py.Tuple.of_pair
;;

let approx_pi =
  let%map_open n = positional_only "n" int ~docstring:"" in
  fun () ->
    let sum =
      List.init n ~f:(fun i ->
        let i = Float.of_int (1 + i) in
        1.0 /. (i *. i))
      |> List.reduce_exn ~f:( +. )
    in
    Float.sqrt (sum *. 6.) |> python_of_float
;;

let map =
  let%map_open list = positional_only "list" (list int) ~docstring:""
  and fn = keyword "fn" (typerep (Function (Int, Int))) ~docstring:"" in
  fun () -> List.map list ~f:fn |> [%python_of: int list]
;;

let () =
  if not (Py.is_initialized ()) then Py.initialize ();
  let mod_ = Py_module.create "example_module" in
  Py_module.set mod_ "add" add;
  Py_module.set mod_ "make_t" make_t;
  Py_module.set mod_ "cartesian_product" cartesian_product;
  Py_module.set mod_ "approx_pi" approx_pi;
  Py_module.set mod_ "map" map;
  Toploop_bindings.register_module ~module_name:"toploop"
;;

(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)
open! Base
open! Python_lib
open! Python_lib.Let_syntax
open! Gen_import

let python_of_ppx_hash_lib__std__hash__state, ppx_hash_lib__std__hash__state_of_python =
  let capsule = lazy (Py.Capsule.make "Ppx_hash_lib.Std.Hash.state") in
  (fun (x : Ppx_hash_lib.Std.Hash.state) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_ppx_hash_lib__std__hash__state =
  Defunc.Of_python.create ~type_name:"Ppx_hash_lib.Std.Hash.state" ~conv:ppx_hash_lib__std__hash__state_of_python
;;

let python_of_core_kernel__date__days__t, core_kernel__date__days__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__Date.Days.t") in
  (fun (x : Core_kernel__Date.Days.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel__date__days__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__Date.Days.t" ~conv:core_kernel__date__days__t_of_python
;;

let python_of_core_kernel____time_float__zone__t, core_kernel____time_float__zone__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__.Time_float.Zone.t") in
  (fun (x : Core_kernel__.Time_float.Zone.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel____time_float__zone__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__.Time_float.Zone.t" ~conv:core_kernel____time_float__zone__t_of_python
;;

let python_of_core_kernel____month__t, core_kernel____month__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__.Month.t") in
  (fun (x : Core_kernel__.Month.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel____month__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__.Month.t" ~conv:core_kernel____month__t_of_python
;;

let python_of_base____formatter__t, base____formatter__t_of_python =
  let capsule = lazy (Py.Capsule.make "Base__.Formatter.t") in
  (fun (x : Base__.Formatter.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_base____formatter__t =
  Defunc.Of_python.create ~type_name:"Base__.Formatter.t" ~conv:base____formatter__t_of_python
;;

let python_of_core_kernel__date__option__t, core_kernel__date__option__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__Date.Option.t") in
  (fun (x : Core_kernel__Date.Option.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel__date__option__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__Date.Option.t" ~conv:core_kernel__date__option__t_of_python
;;

let python_of_core_kernel____time_float__t, core_kernel____time_float__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__.Time_float.t") in
  (fun (x : Core_kernel__.Time_float.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel____time_float__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__.Time_float.t" ~conv:core_kernel____time_float__t_of_python
;;

let python_of_ppx_sexp_conv_lib__sexp__t, ppx_sexp_conv_lib__sexp__t_of_python =
  let capsule = lazy (Py.Capsule.make "Ppx_sexp_conv_lib.Sexp.t") in
  (fun (x : Ppx_sexp_conv_lib.Sexp.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_ppx_sexp_conv_lib__sexp__t =
  Defunc.Of_python.create ~type_name:"Ppx_sexp_conv_lib.Sexp.t" ~conv:ppx_sexp_conv_lib__sexp__t_of_python
;;

let python_of_core_kernel____day_of_week__t, core_kernel____day_of_week__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__.Day_of_week.t") in
  (fun (x : Core_kernel__.Day_of_week.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel____day_of_week__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__.Day_of_week.t" ~conv:core_kernel____day_of_week__t_of_python
;;

let python_of_ppx_hash_lib__std__hash__hash_value, ppx_hash_lib__std__hash__hash_value_of_python =
  let capsule = lazy (Py.Capsule.make "Ppx_hash_lib.Std.Hash.hash_value") in
  (fun (x : Ppx_hash_lib.Std.Hash.hash_value) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_ppx_hash_lib__std__hash__hash_value =
  Defunc.Of_python.create ~type_name:"Ppx_hash_lib.Std.Hash.hash_value" ~conv:ppx_hash_lib__std__hash__hash_value_of_python
;;

let python_of_bin_prot____shape__t, bin_prot____shape__t_of_python =
  let capsule = lazy (Py.Capsule.make "Bin_prot__.Shape.t") in
  (fun (x : Bin_prot__.Shape.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_bin_prot____shape__t =
  Defunc.Of_python.create ~type_name:"Bin_prot__.Shape.t" ~conv:bin_prot____shape__t_of_python
;;

let python_of_core_kernel__date__t, core_kernel__date__t_of_python =
  let capsule = lazy (Py.Capsule.make "Core_kernel__Date.t") in
  (fun (x : Core_kernel__Date.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_core_kernel__date__t =
  Defunc.Of_python.create ~type_name:"Core_kernel__Date.t" ~conv:core_kernel__date__t_of_python
;;

let python_of_sexplib0____sexp__t, sexplib0____sexp__t_of_python =
  let capsule = lazy (Py.Capsule.make "Sexplib0__.Sexp.t") in
  (fun (x : Sexplib0__.Sexp.t) -> (Lazy.force capsule |> fst) x),
  (fun x -> (Lazy.force capsule |> snd) x)
;;
let param_sexplib0____sexp__t =
  Defunc.Of_python.create ~type_name:"Sexplib0__.Sexp.t" ~conv:sexplib0____sexp__t_of_python
;;


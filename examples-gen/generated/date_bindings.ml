(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)
open! Base
open! Python_lib
open! Python_lib.Let_syntax
open! Gen_types
open! Gen_import
[@@@alert "-deprecated-legacy"]

let protect ~f x =
  try f x with
  | Py.Err _ as err -> raise err
  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))
;;

let bin_shape_t () = (* Bin_prot__.Shape.t *)
  Defunc.no_arg (fun () -> Core_kernel__Date.bin_shape_t |> python_of_bin_prot____shape__t)
;;

let t_of_sexp () = (* Sexplib0__.Sexp.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_sexplib0____sexp__t ~docstring:"Sexplib0__.Sexp.t"
  in
  Core_kernel__Date.t_of_sexp
    positional_1
  |> python_of_core_kernel__date__t
;;

let sexp_of_t () = (* Core_kernel__Date.t -> Sexplib0__.Sexp.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.sexp_of_t
    positional_1
  |> python_of_sexplib0____sexp__t
;;

let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core_kernel__Date.t -> Ppx_hash_lib.Std.Hash.state *)
  let%map_open
    positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.hash_fold_t
    positional_1
    positional_2
  |> python_of_ppx_hash_lib__std__hash__state
;;

let hash () = (* Core_kernel__Date.t -> Ppx_hash_lib.Std.Hash.hash_value *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.hash
    positional_1
  |> python_of_ppx_hash_lib__std__hash__hash_value
;;

let of_string () = (* string -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string"
  in
  Core_kernel__Date.of_string
    positional_1
  |> python_of_core_kernel__date__t
;;

let to_string () = (* Core_kernel__Date.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.to_string
    positional_1
  |> python_of_string
;;

let greatereq () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(>=)
    positional_1
    positional_2
  |> python_of_bool
;;

let lowereq () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(<=)
    positional_1
    positional_2
  |> python_of_bool
;;

let eq () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(=)
    positional_1
    positional_2
  |> python_of_bool
;;

let greater () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(>)
    positional_1
    positional_2
  |> python_of_bool
;;

let lower () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(<)
    positional_1
    positional_2
  |> python_of_bool
;;

let neq () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.(<>)
    positional_1
    positional_2
  |> python_of_bool
;;

let equal () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.equal
    positional_1
    positional_2
  |> python_of_bool
;;

let compare () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.compare
    positional_1
    positional_2
  |> python_of_int
;;

let min () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.min
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let max () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.max
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let ascending () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.ascending
    positional_1
    positional_2
  |> python_of_int
;;

let descending () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.descending
    positional_1
    positional_2
  |> python_of_int
;;

let between () = (* Core_kernel__Date.t -> low:Core_kernel__Date.t -> high:Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    low = keyword "low" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    high = keyword "high" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.between
    positional_1
    ~low
    ~high
  |> python_of_bool
;;

let clamp_exn () = (* Core_kernel__Date.t -> min:Core_kernel__Date.t -> max:Core_kernel__Date.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    min = keyword "min" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    max = keyword "max" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.clamp_exn
    positional_1
    ~min
    ~max
  |> python_of_core_kernel__date__t
;;

let pp () = (* Base__.Formatter.t -> Core_kernel__Date.t -> unit *)
  let%map_open
    positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.pp
    positional_1
    positional_2
  |> python_of_unit
;;

let create_exn () = (* y:int -> m:Core_kernel__.Month.t -> d:int -> Core_kernel__Date.t *)
  let%map_open
    y = keyword "y" int ~docstring:"int" and
    m = keyword "m" param_core_kernel____month__t ~docstring:"Core_kernel__.Month.t" and
    d = keyword "d" int ~docstring:"int"
  in
  Core_kernel__Date.create_exn
    ~y
    ~m
    ~d
  |> python_of_core_kernel__date__t
;;

let of_string_iso8601_basic () = (* string -> pos:int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string" and
    pos = keyword "pos" int ~docstring:"int"
  in
  Core_kernel__Date.of_string_iso8601_basic
    positional_1
    ~pos
  |> python_of_core_kernel__date__t
;;

let to_string_iso8601_basic () = (* Core_kernel__Date.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.to_string_iso8601_basic
    positional_1
  |> python_of_string
;;

let to_string_american () = (* Core_kernel__Date.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.to_string_american
    positional_1
  |> python_of_string
;;

let day () = (* Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.day
    positional_1
  |> python_of_int
;;

let month () = (* Core_kernel__Date.t -> Core_kernel__.Month.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.month
    positional_1
  |> python_of_core_kernel____month__t
;;

let year () = (* Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.year
    positional_1
  |> python_of_int
;;

let day_of_week () = (* Core_kernel__Date.t -> Core_kernel__.Day_of_week.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.day_of_week
    positional_1
  |> python_of_core_kernel____day_of_week__t
;;

let week_number_and_year () = (* Core_kernel__Date.t -> (int, int) *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.week_number_and_year
    positional_1
  |> (fun (t0, t1) -> Py.Tuple.of_list [python_of_int t0; python_of_int t1])
;;

let week_number () = (* Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.week_number
    positional_1
  |> python_of_int
;;

let is_weekend () = (* Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.is_weekend
    positional_1
  |> python_of_bool
;;

let is_weekday () = (* Core_kernel__Date.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.is_weekday
    positional_1
  |> python_of_bool
;;

let add_days () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_days
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let add_months () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_months
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let add_years () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_years
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let diff () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.diff
    positional_1
    positional_2
  |> python_of_int
;;

let diff_weekdays () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.diff_weekdays
    positional_1
    positional_2
  |> python_of_int
;;

let diff_weekend_days () = (* Core_kernel__Date.t -> Core_kernel__Date.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.diff_weekend_days
    positional_1
    positional_2
  |> python_of_int
;;

let add_weekdays_rounding_backward () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_weekdays_rounding_backward
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let add_weekdays_rounding_forward () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_weekdays_rounding_forward
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let add_weekdays_deprecated () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_weekdays
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let add_weekdays_rounding_in_direction_of_step () = (* Core_kernel__Date.t -> int -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    positional_2 = positional "positional_2" int ~docstring:"int"
  in
  Core_kernel__Date.add_weekdays_rounding_in_direction_of_step
    positional_1
    positional_2
  |> python_of_core_kernel__date__t
;;

let dates_between () = (* min:Core_kernel__Date.t -> max:Core_kernel__Date.t -> Core_kernel__Date.t list *)
  let%map_open
    min = keyword "min" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    max = keyword "max" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.dates_between
    ~min
    ~max
  |> (python_of_list python_of_core_kernel__date__t)
;;

let weekdays_between () = (* min:Core_kernel__Date.t -> max:Core_kernel__Date.t -> Core_kernel__Date.t list *)
  let%map_open
    min = keyword "min" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    max = keyword "max" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.weekdays_between
    ~min
    ~max
  |> (python_of_list python_of_core_kernel__date__t)
;;

let previous_weekday () = (* Core_kernel__Date.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.previous_weekday
    positional_1
  |> python_of_core_kernel__date__t
;;

let following_weekday () = (* Core_kernel__Date.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
  in
  Core_kernel__Date.following_weekday
    positional_1
  |> python_of_core_kernel__date__t
;;

let first_strictly_after () = (* Core_kernel__Date.t -> on:Core_kernel__.Day_of_week.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t" and
    on = keyword "on" param_core_kernel____day_of_week__t ~docstring:"Core_kernel__.Day_of_week.t"
  in
  Core_kernel__Date.first_strictly_after
    positional_1
    ~on
  |> python_of_core_kernel__date__t
;;

let days_in_month () = (* year:int -> month:Core_kernel__.Month.t -> int *)
  let%map_open
    year = keyword "year" int ~docstring:"int" and
    month = keyword "month" param_core_kernel____month__t ~docstring:"Core_kernel__.Month.t"
  in
  Core_kernel__Date.days_in_month
    ~year
    ~month
  |> python_of_int
;;

let is_leap_year () = (* year:int -> bool *)
  let%map_open
    year = keyword "year" int ~docstring:"int"
  in
  Core_kernel__Date.is_leap_year
    ~year
  |> python_of_bool
;;

let unix_epoch () = (* Core_kernel__Date.t *)
  Defunc.no_arg (fun () -> Core_kernel__Date.unix_epoch |> python_of_core_kernel__date__t)
;;

module Days = struct
  let of_date () = (* Core_kernel__Date.t -> Core_kernel__Date.Days.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
    in
    Core_kernel__Date.Days.of_date
      positional_1
    |> python_of_core_kernel__date__days__t
  ;;

  let to_date () = (* Core_kernel__Date.Days.t -> Core_kernel__Date.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__days__t ~docstring:"Core_kernel__Date.Days.t"
    in
    Core_kernel__Date.Days.to_date
      positional_1
    |> python_of_core_kernel__date__t
  ;;

  let diff () = (* Core_kernel__Date.Days.t -> Core_kernel__Date.Days.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__days__t ~docstring:"Core_kernel__Date.Days.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__days__t ~docstring:"Core_kernel__Date.Days.t"
    in
    Core_kernel__Date.Days.diff
      positional_1
      positional_2
    |> python_of_int
  ;;

  let add_days () = (* Core_kernel__Date.Days.t -> int -> Core_kernel__Date.Days.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__days__t ~docstring:"Core_kernel__Date.Days.t" and
      positional_2 = positional "positional_2" int ~docstring:"int"
    in
    Core_kernel__Date.Days.add_days
      positional_1
      positional_2
    |> python_of_core_kernel__date__days__t
  ;;

  let unix_epoch () = (* Core_kernel__Date.Days.t *)
    Defunc.no_arg (fun () -> Core_kernel__Date.Days.unix_epoch |> python_of_core_kernel__date__days__t)
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "of_date" (of_date ());
    Py_module.set modl "to_date" (to_date ());
    Py_module.set modl "diff" (diff ());
    Py_module.set modl "add_days" (add_days ());
    Py_module.set modl "unix_epoch" (unix_epoch ());
    modl
end;;
module Option = struct
  let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core_kernel__Date.Option.t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open
      positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.hash_fold_t
      positional_1
      positional_2
    |> python_of_ppx_hash_lib__std__hash__state
  ;;

  let hash () = (* Core_kernel__Date.Option.t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.hash
      positional_1
    |> python_of_ppx_hash_lib__std__hash__hash_value
  ;;

  let sexp_of_t () = (* Core_kernel__Date.Option.t -> Ppx_sexp_conv_lib.Sexp.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.sexp_of_t
      positional_1
    |> python_of_ppx_sexp_conv_lib__sexp__t
  ;;

  let none () = (* Core_kernel__Date.Option.t *)
    Defunc.no_arg (fun () -> Core_kernel__Date.Option.none |> python_of_core_kernel__date__option__t)
  ;;

  let some () = (* Core_kernel__Date.t -> Core_kernel__Date.Option.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
    in
    Core_kernel__Date.Option.some
      positional_1
    |> python_of_core_kernel__date__option__t
  ;;

  let some_is_representable () = (* Core_kernel__Date.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
    in
    Core_kernel__Date.Option.some_is_representable
      positional_1
    |> python_of_bool
  ;;

  let is_none () = (* Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.is_none
      positional_1
    |> python_of_bool
  ;;

  let is_some () = (* Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.is_some
      positional_1
    |> python_of_bool
  ;;

  let value () = (* Core_kernel__Date.Option.t -> default:Core_kernel__Date.t -> Core_kernel__Date.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      default = keyword "default" param_core_kernel__date__t ~docstring:"Core_kernel__Date.t"
    in
    Core_kernel__Date.Option.value
      positional_1
      ~default
    |> python_of_core_kernel__date__t
  ;;

  let value_exn () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.value_exn
      positional_1
    |> python_of_core_kernel__date__t
  ;;

  let unchecked_value () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.unchecked_value
      positional_1
    |> python_of_core_kernel__date__t
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none () = (* Core_kernel__Date.Option.t -> bool *)
        let%map_open
          positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
        in
        Core_kernel__Date.Option.Optional_syntax.Optional_syntax.is_none
          positional_1
        |> python_of_bool
      ;;

      let unsafe_value () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.t *)
        let%map_open
          positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
        in
        Core_kernel__Date.Option.Optional_syntax.Optional_syntax.unsafe_value
          positional_1
        |> python_of_core_kernel__date__t
      ;;


      let register_module ~module_name =
        let modl = Py_module.create module_name in
        Py_module.set modl "is_none" (is_none ());
        Py_module.set modl "unsafe_value" (unsafe_value ());
        modl
    end;;

    let register_module ~module_name =
      let modl = Py_module.create module_name in
      let sub_module = Optional_syntax.register_module ~module_name:"core_kernel__date__option__optional_syntax__optional_syntax__optional_syntax" in
      Py_module.set_value modl "optional_syntax" (Py_module.pyobject sub_module);
      modl
  end;;
  let greatereq () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(>=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(<=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(<)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.(<>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let equal () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.equal
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let compare () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let min () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.min
      positional_1
      positional_2
    |> python_of_core_kernel__date__option__t
  ;;

  let max () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.max
      positional_1
      positional_2
    |> python_of_core_kernel__date__option__t
  ;;

  let ascending () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.ascending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let descending () = (* Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      positional_2 = positional "positional_2" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.descending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let between () = (* Core_kernel__Date.Option.t -> low:Core_kernel__Date.Option.t -> high:Core_kernel__Date.Option.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      low = keyword "low" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      high = keyword "high" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.between
      positional_1
      ~low
      ~high
    |> python_of_bool
  ;;

  let clamp_exn () = (* Core_kernel__Date.Option.t -> min:Core_kernel__Date.Option.t -> max:Core_kernel__Date.Option.t -> Core_kernel__Date.Option.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      min = keyword "min" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t" and
      max = keyword "max" param_core_kernel__date__option__t ~docstring:"Core_kernel__Date.Option.t"
    in
    Core_kernel__Date.Option.clamp_exn
      positional_1
      ~min
      ~max
    |> python_of_core_kernel__date__option__t
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "hash_fold_t" (hash_fold_t ());
    Py_module.set modl "hash" (hash ());
    Py_module.set modl "sexp_of_t" (sexp_of_t ());
    Py_module.set modl "none" (none ());
    Py_module.set modl "some" (some ());
    Py_module.set modl "some_is_representable" (some_is_representable ());
    Py_module.set modl "is_none" (is_none ());
    Py_module.set modl "is_some" (is_some ());
    Py_module.set modl "value" (value ());
    Py_module.set modl "value_exn" (value_exn ());
    Py_module.set modl "unchecked_value" (unchecked_value ());
    let sub_module = Optional_syntax.register_module ~module_name:"core_kernel__date__option__optional_syntax__optional_syntax" in
    Py_module.set_value modl "optional_syntax" (Py_module.pyobject sub_module);
    Py_module.set modl "greatereq" (greatereq ());
    Py_module.set modl "lowereq" (lowereq ());
    Py_module.set modl "eq" (eq ());
    Py_module.set modl "greater" (greater ());
    Py_module.set modl "lower" (lower ());
    Py_module.set modl "neq" (neq ());
    Py_module.set modl "equal" (equal ());
    Py_module.set modl "compare" (compare ());
    Py_module.set modl "min" (min ());
    Py_module.set modl "max" (max ());
    Py_module.set modl "ascending" (ascending ());
    Py_module.set modl "descending" (descending ());
    Py_module.set modl "between" (between ());
    Py_module.set modl "clamp_exn" (clamp_exn ());
    modl
end;;
let of_time () = (* Core_kernel__.Time_float.t -> zone:Core_kernel__.Time_float.Zone.t -> Core_kernel__Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core_kernel____time_float__t ~docstring:"Core_kernel__.Time_float.t" and
    zone = keyword "zone" param_core_kernel____time_float__zone__t ~docstring:"Core_kernel__.Time_float.Zone.t"
  in
  Core_kernel__Date.of_time
    positional_1
    ~zone
  |> python_of_core_kernel__date__t
;;

let today () = (* zone:Core_kernel__.Time_float.Zone.t -> Core_kernel__Date.t *)
  let%map_open
    zone = keyword "zone" param_core_kernel____time_float__zone__t ~docstring:"Core_kernel__.Time_float.Zone.t"
  in
  Core_kernel__Date.today
    ~zone
  |> python_of_core_kernel__date__t
;;


let register_module ~module_name =
  let modl = Py_module.create module_name in
  Py_module.set modl "bin_shape_t" (bin_shape_t ());
  Py_module.set modl "t_of_sexp" (t_of_sexp ());
  Py_module.set modl "sexp_of_t" (sexp_of_t ());
  Py_module.set modl "hash_fold_t" (hash_fold_t ());
  Py_module.set modl "hash" (hash ());
  Py_module.set modl "of_string" (of_string ());
  Py_module.set modl "to_string" (to_string ());
  Py_module.set modl "greatereq" (greatereq ());
  Py_module.set modl "lowereq" (lowereq ());
  Py_module.set modl "eq" (eq ());
  Py_module.set modl "greater" (greater ());
  Py_module.set modl "lower" (lower ());
  Py_module.set modl "neq" (neq ());
  Py_module.set modl "equal" (equal ());
  Py_module.set modl "compare" (compare ());
  Py_module.set modl "min" (min ());
  Py_module.set modl "max" (max ());
  Py_module.set modl "ascending" (ascending ());
  Py_module.set modl "descending" (descending ());
  Py_module.set modl "between" (between ());
  Py_module.set modl "clamp_exn" (clamp_exn ());
  Py_module.set modl "pp" (pp ());
  Py_module.set modl "create_exn" (create_exn ());
  Py_module.set modl "of_string_iso8601_basic" (of_string_iso8601_basic ());
  Py_module.set modl "to_string_iso8601_basic" (to_string_iso8601_basic ());
  Py_module.set modl "to_string_american" (to_string_american ());
  Py_module.set modl "day" (day ());
  Py_module.set modl "month" (month ());
  Py_module.set modl "year" (year ());
  Py_module.set modl "day_of_week" (day_of_week ());
  Py_module.set modl "week_number_and_year" (week_number_and_year ());
  Py_module.set modl "week_number" (week_number ());
  Py_module.set modl "is_weekend" (is_weekend ());
  Py_module.set modl "is_weekday" (is_weekday ());
  Py_module.set modl "add_days" (add_days ());
  Py_module.set modl "add_months" (add_months ());
  Py_module.set modl "add_years" (add_years ());
  Py_module.set modl "diff" (diff ());
  Py_module.set modl "diff_weekdays" (diff_weekdays ());
  Py_module.set modl "diff_weekend_days" (diff_weekend_days ());
  Py_module.set modl "add_weekdays_rounding_backward" (add_weekdays_rounding_backward ());
  Py_module.set modl "add_weekdays_rounding_forward" (add_weekdays_rounding_forward ());
  Py_module.set modl "add_weekdays_deprecated" (add_weekdays_deprecated ());
  Py_module.set modl "add_weekdays_rounding_in_direction_of_step" (add_weekdays_rounding_in_direction_of_step ());
  Py_module.set modl "dates_between" (dates_between ());
  Py_module.set modl "weekdays_between" (weekdays_between ());
  Py_module.set modl "previous_weekday" (previous_weekday ());
  Py_module.set modl "following_weekday" (following_weekday ());
  Py_module.set modl "first_strictly_after" (first_strictly_after ());
  Py_module.set modl "days_in_month" (days_in_month ());
  Py_module.set modl "is_leap_year" (is_leap_year ());
  Py_module.set modl "unix_epoch" (unix_epoch ());
  let sub_module = Days.register_module ~module_name:"core_kernel__date__days__days" in
  Py_module.set_value modl "days" (Py_module.pyobject sub_module);
  let sub_module = Option.register_module ~module_name:"core_kernel__date__option__option" in
  Py_module.set_value modl "option" (Py_module.pyobject sub_module);
  Py_module.set modl "of_time" (of_time ());
  Py_module.set modl "today" (today ());
  modl

open Base
open Stdio

module To_be_registered = struct
  type t =
    | Function of string
    | Type of
        { type_name : string
        ; path : Module_env.Path.t
        }
    | Module of
        { ml_module_name : string
        ; path : Module_env.Path.t
        }
end

let do_not_gen_types = Set.of_list (module String) [ "comparator_witness" ]

let primitive_params =
  Set.of_list (module String) [ "bool"; "int"; "string"; "float"; "unit" ]
;;

let do_not_gen_modules =
  Set.of_list
    (module String)
    [ "Table"
    ; "Hash_set"
    ; "Hash_queue"
    ; "Replace_polymorphic_compare"
    ; "Map"
    ; "Set"
    ; "Stable"
    ; "O"
    ; "Private"
    ]
;;

let ops =
  Map.of_alist_exn
    (module String)
    [ "<=", "lowereq"
    ; ">=", "greatereq"
    ; "<", "lower"
    ; ">", "greater"
    ; "=", "eq"
    ; "<>", "neq"
    ; "==", "physeq"
    ; "!=", "nphyseq"
    ; "<=.", "lowereq_approx"
    ; ">=.", "greatereq_approx"
    ; "<.", "lower_approx"
    ; ">.", "greater_approx"
    ; "=.", "eq_approx"
    ; "<>.", "neq_approx"
    ; "==.", "physeq_approx"
    ; "!=.", "nphyseq_approx"
    ; "+", "plus"
    ; "-", "minus"
    ; "*", "times"
    ; "/", "divide"
    ; "//", "divide_"
    ]
;;

module Deprecated_handling = struct
  let is_deprecated (value_description : Types.value_description) =
    value_description.val_attributes
    |> List.exists ~f:(fun { Parsetree.attr_name; _ } ->
      String.equal attr_name.txt "deprecated" || String.equal attr_name.txt "legacy")
  ;;

  let python_name s ~is_deprecated =
    let name = Map.find ops s |> Option.value ~default:s in
    if is_deprecated then name ^ "_deprecated" else name
  ;;

  let ignore_warnings = {|[@@@alert "-deprecated-legacy"]|}
end

let ocaml_name s = if Map.mem ops s then "(" ^ s ^ ")" else s

let escape str ~path =
  Module_env.Path.append path str
  |> Module_env.Path.names
  |> String.concat ~sep:"."
  |> String.substr_replace_all ~pattern:"." ~with_:"__"
  |> String.lowercase
;;

let python_of_type st ~all_types =
  let rec walk : Type.t -> string = function
    | Arrow _ -> failwith "Arrow types are not supported"
    | Tuple2 (t1, t2) -> tuple [ t1; t2 ]
    | Tuple3 (t1, t2, t3) -> tuple [ t1; t2; t3 ]
    | Tuple4 (t1, t2, t3, t4) -> tuple [ t1; t2; t3; t4 ]
    | Tuple5 (t1, t2, t3, t4, t5) -> tuple [ t1; t2; t3; t4; t5 ]
    | Atom (path, constr) ->
      Hash_set.add all_types (path, constr);
      "python_of_" ^ escape constr ~path
    | Apply (t, constr) -> Printf.sprintf "(python_of_%s %s)" constr (walk t)
  and tuple ts =
    let names = List.mapi ts ~f:(fun i t -> Printf.sprintf "t%d" i, t) in
    Printf.sprintf
      "(fun (%s) -> Py.Tuple.of_list [%s])"
      (List.map names ~f:fst |> String.concat ~sep:", ")
      (List.map names ~f:(fun (name, t) -> walk t ^ " " ^ name)
       |> String.concat ~sep:"; ")
  in
  walk st
;;

let pr outc ~indent =
  Printf.ksprintf (fun line ->
    if not (String.is_empty line)
    then (
      for _i = 1 to indent * 2 do
        Out_channel.output_char outc ' '
      done;
      Out_channel.output_string outc line);
    Out_channel.output_char outc '\n')
;;

let uncurrify type_ =
  let args, result = Type.uncurrify type_ in
  if Type.contains_arrow result
  || List.exists args ~f:(fun (_, arg) -> Type.contains_arrow arg)
  then Or_error.error_string "arrow in argument or result"
  else Ok (args, result)
;;

let write_function
      ~indent
      ~outc
      ~python_name
      ~env
      ~type_
      ~args
      ~all_types
      ~ocaml_name
      ~result
  =
  let pr s = pr outc ~indent s in
  let path_str =
    Module_env.path env |> Module_env.Path.names |> String.concat ~sep:"."
  in
  pr "let %s () = (* %s *)" python_name (Type.to_string type_);
  pr "  let%%map_open";
  let args =
    let unique_name =
      let already_used_names = Hash_set.create (module String) in
      fun name ->
        let rec loop d =
          let name = name ^ Int.to_string d in
          if Hash_set.mem already_used_names name then loop (d + 1) else name
        in
        if Hash_set.mem already_used_names name then loop 1 else name
    in
    List.mapi args ~f:(fun i (arg, t) ->
      let name, special_kind =
        match arg with
        | Type.Arg.Nolabel ->
          let special_kind =
            match t with
            | Type.Atom (_, "unit") -> `skip_unit
            | _ -> `none
          in
          "positional_" ^ Int.to_string (i + 1), special_kind
        | Labelled l -> unique_name l, `none
        | Optional l -> unique_name l, `none
      in
      name, arg, t, special_kind)
  in
  let nargs = List.length args in
  List.iteri args ~f:(fun index (name, arg, t, special_kind) ->
    match special_kind with
    | `skip_unit -> ()
    | `none ->
      let kind =
        match arg with
        | Nolabel -> "positional"
        | Labelled _ -> "keyword"
        | Optional _ -> "keyword_opt"
      in
      let rec walk t =
        match (t : Type.t) with
        | Atom (path, a) ->
          if Set.mem primitive_params a
          then a
          else (
            Hash_set.add all_types (path, a);
            "param_" ^ escape a ~path)
        | Tuple2 (t1, t2) -> Printf.sprintf "pair (%s) (%s)" (walk t1) (walk t2)
        | Tuple3 (t1, t2, t3) ->
          Printf.sprintf "triple (%s) (%s) (%s)" (walk t1) (walk t2) (walk t3)
        | Tuple4 (t1, t2, t3, t4) ->
          Printf.sprintf
            "quadruple (%s) (%s) (%s) (%s)"
            (walk t1)
            (walk t2)
            (walk t3)
            (walk t4)
        | Tuple5 (t1, t2, t3, t4, t5) ->
          Printf.sprintf
            "quintuple (%s) (%s) (%s) (%s) (%s)"
            (walk t1)
            (walk t2)
            (walk t3)
            (walk t4)
            (walk t5)
        | Arrow _ | Apply _ -> "pyobject"
      in
      let suffix = if index < nargs - 1 then " and" else "" in
      pr
        "    %s = %s \"%s\" %s ~docstring:\"%s\"%s"
        name
        kind
        name
        (walk t)
        (Type.to_string t)
        suffix);
  pr "  in";
  pr "  %s.%s" path_str ocaml_name;
  List.iter args ~f:(fun (name, arg, _t, special_kind) ->
    let prefix =
      match arg with
      | Nolabel -> ""
      | Labelled _ -> "~"
      | Optional _ -> "?"
    in
    let name =
      match special_kind with
      | `none -> prefix ^ name
      | `skip_unit -> "()"
    in
    pr "    %s" name);
  pr "  |> %s" (python_of_type result ~all_types);
  pr ";;";
  pr "";
  Some (To_be_registered.Function python_name)
;;

let write_value ident value_description outc ~indent ~env ~all_types =
  let pr s = pr outc ~indent s in
  let path_str =
    Module_env.path env |> Module_env.Path.names |> String.concat ~sep:"."
  in
  match Type.of_type_desc value_description.Types.val_type.desc ~env with
  | Error _err -> None
  | Ok type_ ->
    let python_name =
      Ident.name ident
      |> Deprecated_handling.python_name
           ~is_deprecated:(Deprecated_handling.is_deprecated value_description)
    in
    let ocaml_name = Ident.name ident |> ocaml_name in
    (match uncurrify type_ with
     | Ok ([ (Nolabel, Atom (_, "unit")) ], result) ->
       pr "let %s () = (* %s *)" python_name (Type.to_string type_);
       pr
         "  Defunc.no_arg (fun () -> %s.%s () |> %s)"
         path_str
         ocaml_name
         (python_of_type result ~all_types);
       pr ";;";
       pr "";
       Some (To_be_registered.Function python_name)
     | Ok ((_ :: _ as args), result) ->
       write_function
         ~indent
         ~outc
         ~python_name
         ~env
         ~type_
         ~args
         ~all_types
         ~ocaml_name
         ~result
     | Ok ([], _) ->
       pr "let %s () = (* %s *)" python_name (Type.to_string type_);
       pr
         "  Defunc.no_arg (fun () -> %s.%s |> %s)"
         path_str
         ocaml_name
         (python_of_type type_ ~all_types);
       pr ";;";
       pr "";
       Some (Function python_name)
     | Error _ -> None)
;;

let register_module ts outc ~indent =
  let pr s = pr outc ~indent s in
  pr "let register_module ~module_name =";
  pr "  let modl = Py_module.create module_name in";
  List.iter ts ~f:(function
    | To_be_registered.Type _ -> ()
    | Function func_name ->
      pr "  Py_module.set modl \"%s\" (%s ());" func_name func_name
    | Module { ml_module_name; path } ->
      let python_module_name =
        Module_env.Path.append path ml_module_name
        |> Module_env.Path.names
        |> String.concat ~sep:"__"
        |> String.lowercase
      in
      pr
        "  let sub_module = %s.register_module ~module_name:\"%s\" in"
        ml_module_name
        python_module_name;
      pr
        "  Py_module.set_value modl \"%s\" (Py_module.pyobject sub_module);"
        (String.lowercase ml_module_name));
  pr "  modl"
;;

let write_ml outc (cmi_infos : Cmi_format.cmi_infos) =
  let all_types = Hash_set.Poly.create () in
  let rec walk ~indent ~env (s : Types.signature_item) =
    let pr s = pr outc ~indent s in
    match s with
    | Sig_value (ident, value_description, Exported) ->
      write_value ident value_description outc ~indent ~env ~all_types
    | Sig_value (_ident, _value_description, Hidden) -> None
    | Sig_type (type_ident, _, _, _) ->
      Module_env.add_type env ~type_ident;
      let type_name = Ident.name type_ident in
      if not (Set.mem do_not_gen_types type_name)
      then Some (Type { path = Module_env.path env; type_name })
      else None
    | Sig_typext (_ident, _, _, _) -> None
    | Sig_module (module_ident, _, module_declaration, _, _) ->
      (match module_declaration.md_type with
       | Mty_signature signature ->
         let env = Module_env.enter_module env ~module_ident in
         let module_name = Ident.name module_ident in
         if not (Set.mem do_not_gen_modules module_name)
         then (
           pr "module %s = struct" module_name;
           let ts = List.filter_map signature ~f:(walk ~indent:(indent + 1) ~env) in
           pr "";
           register_module ts outc ~indent:(indent + 1);
           pr "end;;";
           Some (Module { ml_module_name = module_name; path = Module_env.path env }))
         else None
       | Mty_ident _ | Mty_functor _ | Mty_alias _ -> None)
    | Sig_modtype (_ident, _, _)
    | Sig_class (_ident, _, _, _)
    | Sig_class_type (_ident, _, _, _) -> None
  in
  let pr s = pr outc ~indent:0 s in
  pr "(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)";
  pr "open! Base";
  pr "open! Python_lib";
  pr "open! Python_lib.Let_syntax";
  pr "open! Gen_types";
  pr "open! Gen_import";
  pr "%s" Deprecated_handling.ignore_warnings;
  pr "";
  pr "let protect ~f x =";
  pr "  try f x with";
  pr "  | Py.Err _ as err -> raise err";
  pr "  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))";
  pr ";;";
  pr "";
  let env =
    Module_env.create ()
    |> Module_env.enter_module ~module_ident:(Ident.create_local cmi_infos.cmi_name)
  in
  let ts = List.filter_map cmi_infos.cmi_sign ~f:(walk ~indent:0 ~env) in
  pr "";
  register_module ts outc ~indent:0;
  all_types
;;

let write_types outc all_types =
  let pr s = pr outc ~indent:0 s in
  pr "(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)";
  pr "open! Base";
  pr "open! Python_lib";
  pr "open! Python_lib.Let_syntax";
  pr "open! Gen_import";
  pr "";
  Hash_set.iter all_types ~f:(fun (path, typename) ->
    if not (Set.mem primitive_params typename)
    then (
      let ml_name =
        Module_env.Path.append path typename
        |> Module_env.Path.names
        |> String.concat ~sep:"."
      in
      let full_name = escape typename ~path in
      pr "let python_of_%s, %s_of_python =" full_name full_name;
      pr "  let capsule = lazy (Py.Capsule.make \"%s\") in" ml_name;
      pr "  (fun (x : %s) -> (Lazy.force capsule |> fst) x)," ml_name;
      pr "  (fun x -> (Lazy.force capsule |> snd) x)";
      pr ";;";
      pr "let param_%s =" full_name;
      pr
        "  Defunc.Of_python.create ~type_name:\"%s\" ~conv:%s_of_python"
        ml_name
        full_name;
      pr ";;";
      pr ""))
;;

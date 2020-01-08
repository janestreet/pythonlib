open Base
open Import

type t =
  | Atom of string
  | Tuple2 of t * t
  | Tuple3 of t * t * t
  | Tuple4 of t * t * t * t
  | Tuple5 of t * t * t * t * t
  | Arrow of t * t
  | Apply of t * string

let rec of_type_expr : Types.type_expr -> t =
  fun type_expr ->
  match type_expr.desc with
  | Tconstr (path, [], _) -> Atom (Path.last path)
  | Tconstr (path, [ td ], _) -> Apply (of_type_expr td, Path.last path)
  | Tconstr (path, _, _) ->
    raise
      (Py.Err
         ( SyntaxError
         , Printf.sprintf "unsupported type %s with multiple parameters" (Path.last path)
         ))
  | Tarrow ((Nolabel | Labelled _), td1, td2, _c) ->
    Arrow (of_type_expr td1, of_type_expr td2)
  | Tarrow (Optional _, _td1, _td2, _c) ->
    raise (Py.Err (SyntaxError, "optional arguments are not supported"))
  | Ttuple [ td1; td2 ] -> Tuple2 (of_type_expr td1, of_type_expr td2)
  | Ttuple [ td1; td2; td3 ] ->
    Tuple3 (of_type_expr td1, of_type_expr td2, of_type_expr td3)
  | Ttuple [ td1; td2; td3; td4 ] ->
    Tuple4 (of_type_expr td1, of_type_expr td2, of_type_expr td3, of_type_expr td4)
  | Ttuple [ td1; td2; td3; td4; td5 ] ->
    Tuple5
      ( of_type_expr td1
      , of_type_expr td2
      , of_type_expr td3
      , of_type_expr td4
      , of_type_expr td5 )
  | Ttuple _ ->
    raise (Py.Err (SyntaxError, Printf.sprintf "tuple with more than 5 arguments"))
  | Tvar _ -> Atom "pyobject"
  | Tlink td -> of_type_expr td
  | Tsubst td -> of_type_expr td
  | Tnil -> raise (Py.Err (SyntaxError, "unsupported type Tnil"))
  | Tvariant _ -> raise (Py.Err (SyntaxError, "unsupported type Tvariant"))
  | Tunivar _ -> raise (Py.Err (SyntaxError, "unsupported type Tunivar"))
  | Tpoly (_, _) -> raise (Py.Err (SyntaxError, "unsupported type Tpoly"))
  | Tpackage (_, _, _) -> raise (Py.Err (SyntaxError, "unsupported type Tpackage"))
  | Tobject (_, _) -> raise (Py.Err (SyntaxError, "unsupported type Tobject"))
  | Tfield (_, _, _, _) -> raise (Py.Err (SyntaxError, "unsupported type Tfield"))
;;

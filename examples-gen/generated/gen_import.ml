open! Base
open! Python_lib

let python_of_unit () = Py.none
let unit_of_python pyobject =
  if not (Py.is_none pyobject)
  then Py.Type.mismatch "unit" pyobject;
  ()

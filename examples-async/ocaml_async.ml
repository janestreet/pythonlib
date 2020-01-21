open Base
open Python_lib

let () =
  if not (Py.is_initialized ()) then Py.initialize ();
  Async_bindings.register_module ~module_name:"oasync"
;;

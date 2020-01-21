open Core
open Async
open Python_lib
open Python_lib.Let_syntax

let caml_start_scheduler () =
  (* The following prevents async computations from running on thread that are
     not registered with the python runtime.
  *)
  In_thread.When_finished.default := Notify_the_scheduler;
  Clock_ns.every (Time_ns.Span.of_sec 0.4) (fun () ->
    Core.printf "hello from async: %s\n%!" (Time_ns.now () |> Time_ns.to_string));
  never_returns (Scheduler.go ())
;;

let register_module ~module_name =
  Callback.register "caml_start_scheduler" caml_start_scheduler;
  let modl = Py_module.create module_name in
  Py_module.set_unit
    modl
    "every"
    [%map_open
      let span =
        positional "span" string ~docstring:"the interval at which to run the function"
      and fn = positional "fn" pyobject ~docstring:"the function to run" in
      ignore (Py.Object.call_function_obj_args fn [||] : pyobject);
      Clock_ns.every (Time_ns.Span.of_string span) (fun () ->
        Stdio.printf
          "hello from python-ocaml %s\n%!"
          (Time_ns.now () |> Time_ns.to_string);
        (* Calling back into python from the async scheduler which runs
           in a different thread would result in a segfault if we do
           not hold the GIL.
        *)
        Py.Gil.with_lock (fun () ->
          ignore (Py.Callable.to_function fn [||] : pyobject)))]
    ~docstring:"Executes a python closure regularly."
;;

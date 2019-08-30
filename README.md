`pythonlib` makes it easier to write wrappers around ocaml functions
so that they can be called from python.

Example
-------

This example is taken from the `examples` directory. The ocaml code
defines a function that takes as argument an integer n, performs some
computations based on n and return a float value.
This function is attached to a newly defined python module named `ocaml_module`.

```ocaml
open Base

let approx_pi =
  let%map_open.Python_lib n = positional "n" int ~docstring:""
  in
  let sum =
    List.init n ~f:(fun i -> let i = Float.of_int (1 + i) in 1.0 /. (i *. i))
    |> List.reduce_exn ~f:(+.)
  in
  Float.sqrt (sum *. 6.) |> python_of_float

let () =
  if not (Py.is_initialized ())
  then Py.initialize ();
  let mod_ = Py_module.create "ocaml_module" in
  Py_module.set mod_ "approx_pi" approx_pi
```

This code is compiled to a static library `python_ocaml_static.so`.
The python code then loads this library, starts the ocaml runtime which results
in creating the `ocaml_module` module, and then uses the `approx_pi` function.

```python
# Load the static library containing the ocaml runtime and functions.
from ctypes import *
ocaml_library = './python_ocaml_static.so'
ocaml = PyDLL(ocaml_library, RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t(ocaml_library.encode('utf-8'), None)
# Start the ocaml runtime.
ocaml.caml_startup(argv)

# Import the module defined in the ocaml code and run the function.
import ocaml_module
print(ocaml_module.approx_pi(1000))
```

`pythonlib` also handles keyword arguments as well as basic types such as
int, float, string, list, etc.
Further examples can be found in the `examples` directory.

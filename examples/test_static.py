from ctypes import *

ocaml_library = './python_ocaml_static.bc.so'

# Load the DLL with OCaml embedded.
# Note that we load it as [PyDll] rather than [CDLL] as the former keeps the GIL
# around the C(/OCaml) calls, which is needed if they use the python API.
# We also need to set RTLD_GLOBAL so subsequent [Dynlink]ed modules can see the OCaml
# runtime
ocaml = PyDLL(ocaml_library, RTLD_GLOBAL)

# Set up [Sys.argv], and start the ocaml system.  The [dirname] of [argv.(0)] is used to
# find the jane-script configuration files.
argv_t = c_char_p * 2
argv = argv_t(ocaml_library.encode('utf-8'), None)
ocaml.caml_startup(argv)

import ocaml_module

print(ocaml_module.add(42, 42))

print(ocaml_module.make_t("my-t", repeats=3))
print(ocaml_module.make_t("my-t", repeats=3, bar2=2.71828))

print(ocaml_module.cartesian_product([1, 2], [(3, 4), "5"]))

print(ocaml_module.approx_pi(1000))

import ocaml_toploop

ocaml_toploop.eval('Printf.printf "hello from ocaml\n%!";;')
ocaml_fn = ocaml_toploop.get(
    '(int * string) list -> string list',
    'List.map (fun (d, s) -> Printf.sprintf "%d: %s" (d+1) (String.uppercase_ascii s))'
)

line1, line2 = ocaml_fn([(3141592, 'first-line'), (2718281, 'second-line')])
print(line1)
print(line2)

# Note that the ocaml toploop has a persisted state.
ocaml_toploop.eval('let x = 42;;')
x = ocaml_toploop.get('float', 'float_of_int x')
print(x)

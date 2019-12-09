# This tries to copy the generated shared library to ocaml.so in the
# current directory so that the import could work.
import os
import shutil
import sys
for src in ['ocaml.bc.so', '_build/default/examples/ocaml.bc.so']:
  if os.path.exists(src): shutil.copyfile(src, 'ocaml.so')
sys.path.append('.')
from ocaml import example_module, toploop

print(example_module.add(42, 42))

print(example_module.make_t("my-t", repeats=3))
print(example_module.make_t("my-t", repeats=3, bar2=2.71828))

print(example_module.cartesian_product([1, 2], [(3, 4), "5"]))

print(example_module.approx_pi(1000))

toploop.eval('Printf.printf "hello from ocaml\n%!";;')
ocaml_fn = toploop.get(
    '(int * string) list -> string list',
    'List.map (fun (d, s) -> Printf.sprintf "%d: %s" (d+1) (String.uppercase_ascii s))'
)

line1, line2 = ocaml_fn([(3141592, 'first-line'), (2718281, 'second-line')])
print(line1)
print(line2)

# Note that the ocaml toploop has a persisted state.
toploop.eval('let x = 42;;')
x = toploop.get('float', 'float_of_int x')
print(x)

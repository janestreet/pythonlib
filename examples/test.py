
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

print(example_module.map(list(range(5)), fn=lambda x: x*x))

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

fn = toploop.get('((int -> int) * int) -> int', 'fun (f, v) -> f (2 * v) + 1')
print(fn((lambda x: x*x, 5)))

counter = toploop.get('fun () -> let v = ref 0 in fun () -> v := !v + 1; !v')
cnt1 = counter()
cnt2 = counter()
print([cnt1() for v in range(5)], cnt2())

# Type inference can be used to avoid specifying the type.
fn = toploop.get('fun x (y, z) -> Int.to_string x ^ y ^ Int.to_string z')
print(fn(4))
print(fn(4)('abc', 5))

# Polymorphic functions can be used.
pair = toploop.get('fun x -> x, x')
x0, x1 = pair('test')
print(x0, x1)

map_fn = toploop.get('fun (x, f) -> List.map f x')
print(map_fn([1, 2, 3], lambda x: x*x))
print(map_fn.__doc__)

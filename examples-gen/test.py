# This tries to copy the generated shared library to ocaml.so in the
# current directory so that the import could work.
import os, sys
from ctypes import *

for ocaml_library in ['examples-gen/generated/ocaml.bc.so', '_build/default/examples-gen/generated/ocaml.bc.so']:
  if os.path.isfile(ocaml_library):
    break
ocaml = PyDLL(ocaml_library, RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t(ocaml_library.encode('utf-8'), None)
ocaml.caml_startup(argv)

import odate
d = odate.of_string('2019-01-16')
print(d)
d = odate.add_days(d, 365)
print(d)
print(odate.to_string(d))

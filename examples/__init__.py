# This is defining some custom jupyter magics which allow for
# the nice %ocaml and %%ocaml syntax that we get in the notebook.
# See: https://ipython.readthedocs.io/en/stable/config/custommagics.html
import os
from .sharedlib import ocaml
from .sharedlib.ocaml import toploop
_topdir = os.path.dirname(os.path.abspath(ocaml.__file__))
toploop.add_topdir(_topdir)

def _register_ipython_magic():
  try:
    from wurlitzer import sys_pipes
    from IPython.core.magic import register_line_magic, register_cell_magic

    # [@register_line_magic] registers itself as a side effect thus
    # the immediate deletion following the definition

    @register_line_magic
    def ocaml(line):
      with sys_pipes():
        return toploop.get(line)

    del ocaml

    @register_line_magic
    def ocaml_t(line):
      type_, body = line.split(':', maxsplit=1)
      with sys_pipes():
        return toploop.get(type_, body)

    del ocaml_t


    @register_cell_magic
    def ocaml(line, cell):
      with sys_pipes():
        return toploop.eval(cell)

    del ocaml
  except:
    pass

_register_ipython_magic()

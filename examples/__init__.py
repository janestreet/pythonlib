import os
from .sharedlib import ocaml
from .sharedlib.ocaml import toploop
_topdir = os.path.dirname(os.path.abspath(ocaml.__file__))
toploop.add_topdir(_topdir)

def _register_ipython_magic():
  try:
    from wurlitzer import sys_pipes
    from IPython.core.magic import register_line_magic, register_cell_magic

    @register_line_magic
    def ocaml(line):
      type_, body = line.split(':', maxsplit=1)
      with sys_pipes():
        return toploop.get(type_, body)

    del ocaml

    @register_cell_magic
    def ocaml(line, cell):
      with sys_pipes():
        return toploop.eval(cell)

    del ocaml
  except:
    pass

_register_ipython_magic()

#include<stdio.h>
#include<Python.h>

static struct PyModuleDef ocamlmodule = {
  PyModuleDef_HEAD_INIT,
  "ocaml",   /* name of module */
};

PyObject* PyInit_ocaml() {
  static char* argv[2] = { "python", NULL };
  caml_startup(argv);
  PyObject *m = PyModule_Create(&ocamlmodule);
  PyObject *toploop = PyImport_ImportModule("toploop");
  int status = PyModule_AddObject(m, "toploop", toploop);
  if (status != 0) return NULL;
  PyObject *example_module = PyImport_ImportModule("example_module");
  status = PyModule_AddObject(m, "example_module", example_module);
  if (status != 0) return NULL;
  return m;
}

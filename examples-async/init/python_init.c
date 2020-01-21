#include<stdio.h>
#include<caml/callback.h>
#include<caml/mlvalues.h>
#include<caml/threads.h>
#include<Python.h>
#include <pthread.h>

void *threaded_start_scheduler(void *t) {
  int d = caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback(*caml_named_value("caml_start_scheduler"), Val_unit);
  caml_release_runtime_system();
  pthread_exit(0);
}

static PyObject * caml_acquire_runtime_lock(PyObject *self, PyObject *args) {
  caml_acquire_runtime_system();
  return Py_None;
}

static PyObject * caml_release_runtime_lock(PyObject *self, PyObject *args) {
  caml_release_runtime_system();
  return Py_None;
}

static PyMethodDef ocamlmethods[] = {
  {"caml_acquire_runtime_lock", caml_acquire_runtime_lock, METH_VARARGS, "Acquire the ocaml runtime lock."},
  {"caml_release_runtime_lock", caml_release_runtime_lock, METH_VARARGS, "Release the ocaml runtime lock."},
  {NULL, NULL, 0, NULL}
};

static struct PyModuleDef ocamlmodule = {
  PyModuleDef_HEAD_INIT,
  "ocaml",   /* name of module */
  NULL,
  -1,
  ocamlmethods,
};

PyObject* PyInit_ocaml_async() {
  static char* argv[2] = { "python", NULL };
  caml_startup(argv);
  PyEval_InitThreads();
  pthread_t thread;
  int rc = pthread_create(&thread, NULL, threaded_start_scheduler, NULL);
  caml_release_runtime_system();
  PyObject *m = PyModule_Create(&ocamlmodule);
  PyObject *oasync = PyImport_ImportModule("oasync");
  int status = PyModule_AddObject(m, "oasync", oasync);
  if (status != 0) return NULL;
  return m;
}

# This tries to copy the generated shared library to ocaml.so in the
# current directory so that the import could work.
import os
import shutil
import sys
import time
import _thread
for src in ['_build/default/examples-async/ocaml_async.so']:
  if os.path.exists(src): shutil.copyfile(src, 'ocaml_async.so')
sys.path.append('.')

from ocaml_async import oasync

# All the ocaml calls should be wrapped by acquiring and releasing the
# ocaml runtime lock.
# We should keep track of this per thread id, not acquiring the lock if
# the thread already holds it. Ideally this should be done on the C side
# in a similar way as PyGILState_{Ensure,Release}.
class OCamlLock(object):
  _threads_with_lock = set()

  def __init__(self):
    self._thread_id = _thread.get_ident()
    self._should_release_lock = False

  def __enter__(self):
    from ocaml_async import caml_acquire_runtime_lock
    if self._thread_id in OCamlLock._threads_with_lock:
      return
    self._should_release_lock = True
    OCamlLock._threads_with_lock.add(self._thread_id)
    caml_acquire_runtime_lock()

  def __exit__(self, exc_type, exc_value, traceback):
    from ocaml_async import caml_release_runtime_lock
    if not self._should_release_lock:
      return
    OCamlLock._threads_with_lock.remove(self._thread_id)
    caml_release_runtime_lock()

def go():
  print('hello from python->ocaml/async->python', _thread.get_ident(), time.time())

time.sleep(2.)

with OCamlLock():
  oasync.every('0.5s', go)
time.sleep(3.)

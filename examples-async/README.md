
The `ocaml-async` example consists in a python script that starts an ocaml
async scheduler in a new pthread. This way ocaml and python can run
concurrently.

Note that this example is hacky in nature: if the python or ocaml locks
are not handled properly, this is likely to result in a segfault or data
corruption.

The example can be run via the following commands:
```bash
dune build examples-async/ocaml_async.so
python examples-async/test.py
```

open Import
open Base

type t

(** [create ?docstring module_name] creates a new python module with the specified name.
    This module can directly be imported from python.
*)
val create : ?docstring:string -> string -> t

(** [module_name t] returns the name used to create the module. *)
val module_name : t -> string

(** [create_with_eval ~name ~py_source] creates a new module defined by evaluating
    [py_source] which contains some python code.
*)
val create_with_eval : name:string -> py_source:string -> t

(** [import module_name] imports an already existing module. *)
val import : string -> t

(** [set_value t name obj] sets the field [name] on module [t] to hold value
    [obj]. This can be accessed via [t.name] in python.
*)
val set_value : t -> string -> pyobject -> unit

(** [pyobject t] returns the underlying python object for a module. *)
val pyobject : t -> pyobject

(** [set_function t ?docstring name fn] adds to module [t] a function named [name]
    which evaluates as the [fn] closure.
    This only handles positional arguments.
*)
val set_function
  :  t
  -> ?docstring:string
  -> string
  -> (pyobject array -> pyobject)
  -> unit

(** [set_function_with_keywords t ?docstring name fn] adds to module [t]
    a function named [name] which evaluates as the [fn] closure.
    This handles both positional and keyword arguments.
*)
val set_function_with_keywords
  :  t
  -> ?docstring:string
  -> string
  -> (pyobject array -> (string, pyobject, String.comparator_witness) Map.t -> pyobject)
  -> unit

(** [set t ?docstring name fn] sets a function on module [t] named [name]. This
    function is defined by defunc [fn].
*)
val set : t -> ?docstring:string -> string -> (unit -> pyobject) Defunc.t -> unit

(** [set_unit] is a specialized version of [set] for function that return [unit].
*)
val set_unit : t -> ?docstring:string -> string -> (unit -> unit) Defunc.t -> unit

(** [set_no_arg t ?docstring name fn] sets a function on module [t] named [name]. This
    function does not take any positional or keyword argument.
*)
val set_no_arg : t -> ?docstring:string -> string -> (unit -> pyobject) -> unit

(** Helper function to get keywords from a python object.
    When no keyword is present, null is used; otherwise a
    python dictionary with string key gets used.
*)
val keywords_of_python
  :  pyobject
  -> (string, pyobject, String.comparator_witness) Map.t Or_error.t

val wrap_ocaml_errors : (unit -> 'a) -> 'a

(** Raise a Python exception that will include the OCaml backtrace information as part
    of the Python exception traceback.
    [unwrap_more] can be specified to extract nested exceptions and provide additional
    backtraces if relevant. Note that [unwrap_more exn] returning [exn] would result
    in an infinite loop.
*)
val raise_py_err_with_backtrace
  :  ?unwrap_more:(exn -> (Stdlib.Printexc.raw_backtrace list * exn) option)
  -> ?backtrace:Stdlib.Printexc.raw_backtrace
  -> exn
  -> 'a

module Raw : sig
  val set : pyobject -> ?docstring:string -> string -> (unit -> pyobject) Defunc.t -> unit
end

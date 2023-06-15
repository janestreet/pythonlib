open! Core


(** [Py_traceback] is a wrapper around [Py.Traceback] which provides more functionalities
    to Python/OCaml traceback manipulation. *)

include module type of Py.Traceback

module Unstable : sig
  type nonrec frame = frame =
    { filename : string
    ; function_name : string
    ; line_number : int
    }
  [@@deriving bin_io, sexp]

  type nonrec t = t [@@deriving bin_io, sexp]
end

(** Raise a Python exception that will include the OCaml backtrace information as part
    of the Python exception traceback.
    [unwrap_more] can be specified to extract nested exceptions and provide additional
    backtraces if relevant. Note that [unwrap_more exn] returning [exn] would raise. *)
val raise_py_err_with_backtrace
  :  ?unwrap_more:(exn -> (Stdlib.Printexc.raw_backtrace list * exn) option)
  -> ?backtrace:Stdlib.Printexc.raw_backtrace
  -> exn
  -> 'a

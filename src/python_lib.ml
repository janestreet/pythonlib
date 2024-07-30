include Import
module Broadcast = Broadcast
module Class_wrapper = Class_wrapper
module Defunc = Defunc
module Module_env = Module_env
module Py_module = Py_module
module Py_typerep = Py_typerep
module Type = Type

module Let_syntax = struct
  include Defunc.Param

  module Let_syntax = struct
    include Defunc
    module Open_on_rhs = Defunc.Param
  end
end

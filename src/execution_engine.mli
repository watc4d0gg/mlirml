open Ctypes
open Bindings.Types
open Ir.Ir

module ExecutionEngine : sig
  type argument = Arg : 'a * 'a typ -> argument

  class type t = object
    method lookup : string -> unit ptr option
    method register_symbol : 'a. string -> 'a -> unit
    method destroy : unit
    method dump_to_object_file : string -> unit
    method raw : MlirExecutionEngine.t structure
  end

  val from_raw : MlirExecutionEngine.t structure -> t

  val get : Module.t -> int -> string list -> bool -> t
end
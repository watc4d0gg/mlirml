open Ir.Ir

module Tensor : sig
  val empty : #Value.t list -> #Type.t -> Location.t -> Operation.t
end

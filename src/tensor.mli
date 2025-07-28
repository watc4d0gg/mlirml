open Ir.Ir

module Tensor : sig
  val empty : #Value.t list -> #Type.t -> Location.t -> Operation.t
  val insert : #Value.t -> #Value.t -> #Value.t list -> Location.t -> Operation.t
end

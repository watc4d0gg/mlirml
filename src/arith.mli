open Ir.Ir

module Arith : sig
  (** @canonical Mlir.Arith *)

  val negf : #Value.t -> Location.t -> Operation.t
  val addi : #Value.t -> #Value.t -> Location.t -> Operation.t
  val addf : #Value.t -> #Value.t -> Location.t -> Operation.t
  val subf : #Value.t -> #Value.t -> Location.t -> Operation.t
  val mulf : #Value.t -> #Value.t -> Location.t -> Operation.t
  val constant : #Attribute.t -> Location.t -> Context.t -> Operation.t
end

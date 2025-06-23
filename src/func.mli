open Ir.Ir
open Builtin_attributes

(** @canonical Mlir.Func *)
module Func : sig
  val func
    :  Context.t
    -> StringAttr.t
    -> #TypedAttr.t
    -> (Identifier.t * #Attribute.t) list
    -> init:(Block.t -> unit)
    -> Location.t
    -> Operation.t

  val return : #Value.t list -> Location.t -> Operation.t
end

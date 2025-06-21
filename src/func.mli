open Ir.Ir
open Builtin_attributes

module Func : sig
  val func
    :  Context.t
    -> StringAttr.t
    -> #TypedAttr.t
    -> Region.t
    -> (Identifier.t * #Attribute.t) list
    -> Location.t
    -> Operation.t

  val return : #Value.t list -> Location.t -> Operation.t
end

open Ir.Ir
open Affine_map.AffineMap

module Linalg : sig
  val generic
    :  Context.t
    -> #Value.t list
    -> #Value.t list
    -> AffineMap.t list
    -> string list
    -> init:(Block.t -> #Value.t list)
    -> Location.t
    -> Operation.t

  val yield : #Value.t list -> Location.t -> Operation.t
end

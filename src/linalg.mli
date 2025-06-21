open Ir.Ir
open Affine_map.AffineMap

module Linalg : sig
  val generic
    :  Context.t
    -> #Value.t list
    -> #Value.t list
    -> AffineMap.t list
    -> string list
    -> init:(Block.t -> unit)
    -> Location.t
    -> Operation.t
end

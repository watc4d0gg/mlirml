module Bindings : sig
  include module type of Bindings.Types (** @inline *)
end

include module type of Support

include module type of Ir (** @inline *)

include module type of Affine_map.AffineMap
include module type of Builtin_attributes
include module type of Builtin_types

include module type of Pass

include module type of Arith (** @inline *)

include module type of Func (** @inline *)

include module type of Sparse_tensor (** @inline *)

include module type of Tensor (** @inline *)

include module type of Linalg (** @inline *)

val print_as_string : (callback:(string -> unit) -> 'a -> unit) -> 'a -> string
val with_context : Ir.Context.t -> (Ir.Context.t -> 'a) -> 'a
val register_all_dialects : Ir.DialectRegistry.t -> unit
val register_all_llvm_translations : Ir.Context.t -> unit
val register_all_passes : unit -> unit

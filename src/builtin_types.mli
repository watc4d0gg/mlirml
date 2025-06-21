open Ctypes
open Bindings.Types
open Ir.Ir

module Function : sig
  class type t = object
    inherit Type.t
    method input : int -> Type.t
    method iter_inputs : f:(Type.t -> unit) -> unit
    method iteri_inputs : f:(int -> Type.t -> unit) -> unit
    method result : int -> Type.t
    method iter_results : f:(Type.t -> unit) -> unit
    method iteri_results : f:(int -> Type.t -> unit) -> unit
  end

  val from_raw : MlirType.t structure -> t
  val get : Context.t -> #Type.t list -> #Type.t list -> t
end

module Shaped : sig
  type dim_size =
    | Static of int
    | Dynamic

  class type t = object
    inherit Type.t

    (** [element_type] returns the element type of the shaped type *)
    method element_type : Type.t

    (** [has_rank] checks whether the given shaped type is ranked *)
    method has_rank : bool

    (** [rank] returns the rank of the given ranked shaped type *)
    method rank : int option

    (** [has_static_shape] checks whether the given shaped type has a static shape *)
    method has_static_shape : bool

    (** [is_dynamic_dimension dim] checks whether the [dim]-th dimension of the given shaped type is dynamic *)
    method is_dynamic_dimension : int -> bool

    (** [dimension_size dim] returns the [dim]-th dimension of the given ranked shaped type, or nothing if the shaped type is unranked *)
    method dimension_size : int -> dim_size option

    (* TODO: dynamic strides and offsets *)
  end

  val cast : #Type.t -> t
end

module Tensor : sig
  class type t = object
    inherit Shaped.t
  end

  (** [is_unranked] checks whether the given type is an unranked tensor type *)
  val is_unranked : #Type.t -> bool

  (** [is_ranked] checks whether the given type is a ranked tensor type *)
  val is_ranked : #Type.t -> bool
end

module RankedTensor : sig
  class type t = object
    inherit Tensor.t

    (** [encoding] gets the 'encoding' attribute from the ranked tensor type, returning a nothing if none is present *)
    method encoding : Attribute.t option
  end

  val from_raw : MlirType.t structure -> t

  (** [get dims element encoding location] creates a tensor type of a fixed rank with the given shape, element type,
    and optional encoding in the same context as the element type. The type is owned by the context. On illegal arguments,
    the function emits appropriate diagnostics and throws a runtime error. *)
  val get : Shaped.dim_size list -> #Type.t -> #Attribute.t option -> Location.t -> t
end

module UnrankedTensor : sig
  class type t = object
    inherit Tensor.t
  end

  val from_raw : MlirType.t structure -> t

  (** [get element location] creates an unranked tensor type with the given element type in the same
    context as the element type. The type is owned by the context. On illegal arguments,
    the function emits appropriate diagnostics and a runtime error. *)
  val get : #Type.t -> Location.t -> t
end

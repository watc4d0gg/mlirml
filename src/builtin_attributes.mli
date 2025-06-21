open Ctypes
open Bindings.Types
open Affine_map.AffineMap
open Ir.Ir

module AffineMapAttr : sig
  class type t = object
    inherit Attribute.t

    (** [value] returns the affine map wrapped in the given affine map attribute *)
    method value : AffineMap.t
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get map] creates an affine map attribute wrapping the given [map]. The attribute
      belongs to the same context as the affine map *)
  val get : AffineMap.t -> t
end

module ArrayAttr : sig
  class type t = object
    inherit Attribute.t

    (** [elements] returns the number of elements stored in the given array attribute *)
    method elements : int

    (** [element pos] returns [pos]-th element stored in the given array attribute *)
    method element : int -> Attribute.t

    (** [iter_elements f] iterates over the elements stored in the given array attribute,
        and applies [f] on each of them *)
    method iter_elements : f:(Attribute.t -> unit) -> unit

    (** [iteri_elements f] iterates over the elements stored in the given array attribute,
        and applies [f] on each of them and their corresponding indices *)
    method iteri_elements : f:(int -> Attribute.t -> unit) -> unit
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx attrs] creates an array element containing the given list of [elems] in the given [ctx] *)
  val get : Context.t -> #Attribute.t list -> t
end

module TypedAttr : sig
  class type t = object
    inherit Attribute.t

    (** [value] returns the type stored in the given type attribute *)
    method value : Type.t
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get t] creates a type attribute wrapping the given [t] in the same context as the type *)
  val get : #Type.t -> t
end

module FloatAttr : sig
  class type t = object
    inherit Attribute.t

    (** [value] returns the value stored in the given floating point attribute,
      interpreting the value as double (float in OCaml) *)
    method value : float
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get t value location] creates a floating point attribute in the given context with the given
    double value and double-precision FP semantics (float value in OCaml). If the [t] is not valid for a
    construction of a FloatAttr, throw a runtime error *)
  val get : #Type.t -> float -> Location.t -> t
end

module StringAttr : sig
  class type t = object
    inherit Attribute.t

    (** [value] returns the attribute values as a string reference. The data remains live as
        long as the context in which the attribute lives *)
    method value : string
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx value] creates a string attribute in the given [ctx] containing the given [value] *)
  val get : Context.t -> string -> t

  (** [get ctx value] creates a string attribute in the given [ctx] containing the given [value].
      Additionally, the attribute has the given type. *)
  val get_typed : #Type.t -> string -> t
end

module DenseArrayAttr : sig
  class type ['a] t = object
    inherit Attribute.t

    (** [elements] get the size of a dense array *)
    method elements : int

    (** [element index] get an element of a dense array at [index] *)
    method element : int -> 'a

    (** [iter_elements f] iterates over the elements stored in the given dense array attribute,
        and applies [f] on each of them *)
    method iter_elements : f:('a -> unit) -> unit

    (** [iteri_elements f] iterates over the elements stored in the given dense array attribute,
        and applies [f] on each of them and their corresponding indices *)
    method iteri_elements : f:(int -> 'a -> unit) -> unit
  end
end

module DenseBoolArrayAttr : sig
  class type t = [bool] DenseArrayAttr.t

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx elements] create a dense boolean array attribute with the given [elements] *)
  val get : Context.t -> bool list -> t
end

module DenseInt8ArrayAttr : sig
  class type t = [int] DenseArrayAttr.t

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx elements] create a dense 8-bit integer array attribute with the given [elements] *)
  val get : Context.t -> int list -> t
end

module DenseInt16ArrayAttr : sig
  class type t = [int] DenseArrayAttr.t

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx elements] create a dense 16-bit integer array attribute with the given [elements] *)
  val get : Context.t -> int list -> t
end

module DenseInt32ArrayAttr : sig
  class type t = [int32] DenseArrayAttr.t

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx elements] create a dense 32-bit integer array attribute with the given [elements] *)
  val get : Context.t -> int32 list -> t
end

module DenseInt64ArrayAttr : sig
  class type t = [int64] DenseArrayAttr.t

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx elements] create a dense 64-bit integer array attribute with the given [elements] *)
  val get : Context.t -> int64 list -> t
end

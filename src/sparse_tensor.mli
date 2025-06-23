open Ctypes
open Bindings.Types
open Ir.Ir
open Affine_map.AffineMap

module SparseTensorFormat : sig
  type t =
    | Dense
    | Batch
    | Compressed
    | Singleton
    | LooseCompressed
    | NOutOfM
end

module SparseTensorProperty : sig
  type t =
    | NonUnique
    | NonOrdered
end

module SparseTensorLevelType : sig
  type t

  val build
    :  SparseTensorFormat.t
    -> SparseTensorProperty.t list
    -> int option
    -> int option
    -> t

  val structured_n : t -> int option
  val structured_m : t -> int option
end

module SparseTensorEncodingAttr : sig
  class type t = object
    inherit Attribute.t

    (** [level_rank] returns the level-rank of the `sparse_tensor.encoding` attribute *)
    method level_rank : int

    (** [level_type] returns a specified level-type of the `sparse_tensor.encoding` attribute *)
    method level_type : int -> SparseTensorLevelType.t

    (** [level_format lvl] returns a specified level-format of the `sparse_tensor.encoding` attribute *)
    method level_format : int -> SparseTensorFormat.t

    (** [dimension_to_levels] returns the dimension-to-level mapping of the `sparse_tensor.encoding` attribute *)
    method dimensions_to_levels : AffineMap.t

    (** [levels_to_dimensions] returns the level-to-dimension mapping of the `sparse_tensor.encoding` attribute *)
    method levels_to_dimensions : AffineMap.t

    (** [positions_bit_width] returns the position bitwidth of the `sparse_tensor.encoding` attribute *)
    method positions_bit_width : int

    (** [coordinates_bit_width] returns the coordinate bitwidth of the `sparse_tensor.encoding` attribute *)
    method coordinates_bit_width : int

    (** [explicit_value] returns the explicit value of the `sparse_tensor.encoding` attribute *)
    method explicit_value : Attribute.t

    (** [implicit_value] returns the implicit value of the `sparse_tensor.encoding` attribute *)
    method implicit_value : Attribute.t
  end

  val from_raw : MlirAttribute.t structure -> t

  (** [get ctx lvl_types dim_to_lvl lvl_to_dim pos_width crd_width explicit_val implicit_val] creates a `sparse_tensor.encoding` attribute with the given parameters *)
  val get
    :  Context.t
    -> SparseTensorLevelType.t list
    -> AffineMap.t
    -> AffineMap.t option
    -> int option
    -> int option
    -> #Attribute.t option
    -> #Attribute.t option
    -> t
end

open Ctypes
open Bindings.Types
open Ir.Ir

module rec AffineMap : sig
  module AffineExpr : sig
    class type t = object
      method context : Context.t
      method compose : AffineMap.AffineMap.t -> t
      method raw : MlirAffineExpr.t structure
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val is_symbolic_or_constant : #t -> bool
    val is_pure_affine : #t -> bool
    val largest_known_divisor : #t -> int
    val is_multiple_of : #t -> int -> bool
    val is_function_of_dim : #t -> int -> bool
    val is_binary : #t -> bool
    val equal : #t -> #t -> bool
    val print : callback:(string -> unit) -> #t -> unit
    val dump : #t -> unit
  end

  module AffineDimExpr : sig
    class type t = object
      inherit AffineExpr.t
      method position : int
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : Context.t -> int -> t
  end

  module AffineSymbolExpr : sig
    class type t = object
      inherit AffineExpr.t
      method position : int
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : Context.t -> int -> t
  end

  module AffineConstantExpression : sig
    class type t = object
      inherit AffineExpr.t
      method value : int
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : Context.t -> int -> t
  end

  module AffineAddExpression : sig
    class type t = object
      inherit AffineExpr.t
      method lhs : AffineExpr.t
      method rhs : AffineExpr.t
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : #AffineExpr.t -> #AffineExpr.t -> t
  end

  module AffineMulExpression : sig
    class type t = object
      inherit AffineExpr.t
      method lhs : AffineExpr.t
      method rhs : AffineExpr.t
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : #AffineExpr.t -> #AffineExpr.t -> t
  end

  module AffineModExpression : sig
    class type t = object
      inherit AffineExpr.t
      method lhs : AffineExpr.t
      method rhs : AffineExpr.t
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : #AffineExpr.t -> #AffineExpr.t -> t
  end

  module AffineFloorDivExpression : sig
    class type t = object
      inherit AffineExpr.t
      method lhs : AffineExpr.t
      method rhs : AffineExpr.t
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : #AffineExpr.t -> #AffineExpr.t -> t
  end

  module AffineCeilDivExpression : sig
    class type t = object
      inherit AffineExpr.t
      method lhs : AffineExpr.t
      method rhs : AffineExpr.t
    end

    val from_raw : MlirAffineExpr.t structure -> t
    val get : #AffineExpr.t -> #AffineExpr.t -> t
  end

  module AffineMap : sig
    class type t = object
      method context : Context.t
      method is_identity : bool
      method is_minor_identity : bool
      method is_empty : bool
      method is_constant : bool
      method is_projected_permutation : bool
      method is_permutation : bool
      method constant_result : int option
      method dims : int
      method symbols : int
      method results : int
      method result : int -> AffineExpr.t
      method inputs : int
      method sub_map : int list -> t
      method major_sub_map : int -> t
      method minor_sub_map : int -> t
      method replace : #AffineExpr.t -> #AffineExpr.t -> int -> int -> t
      method raw : MlirAffineMap.t structure
    end

    val from_raw : MlirAffineMap.t structure -> t
    val empty : Context.t -> t
    val zero : Context.t -> int -> int -> t
    val get : Context.t -> int -> int -> #AffineExpr.t list -> t
    val constant : Context.t -> int -> t
    val multi_dim_identity : Context.t -> int -> t
    val minor_identity : Context.t -> int -> int -> t
    val permutation : Context.t -> int list -> t

    (**
    Returns the simplified affine map resulting from dropping the symbols that
    do not appear in any of the individual maps in `affineMaps`.
    Asserts that all maps in `affineMaps` are normalized to the same number of
    dims and symbols.
    *)
    val compress_unused_symbols : t list -> t list

    val equal : t -> t -> bool

    (**
    Prints an affine map by sending chunks of the string representation to `callback`.
    Note that the callback may be called several times
    with consecutive chunks of the string.
    *)
    val print : callback:(string -> unit) -> t -> unit

    val dump : t -> unit
  end
end

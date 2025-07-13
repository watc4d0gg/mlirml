open Ctypes
open Bindings.Types

module LogicalResult : sig
  (** @canonical Mlir.LogicalResult *)

  type t =
    | Success
    | Failure

  val from_raw : MlirLogicalResult.t structure -> t
end

module LLVMThreadPool : sig
  (** @canonical Mlir.LLVMThreadPool *)

  class type t = object
    method destroy : unit
    method raw : MlirLlvmThreadPool.t structure
  end

  val from_raw : MlirLlvmThreadPool.t structure -> t
  val get : unit -> t
end

module TypeId : sig
  (** @canonical Mlir.TypeId *)

  class type t = object
    method hash : Unsigned.size_t
    method raw : MlirTypeID.t structure
  end

  val from_raw : MlirTypeID.t structure -> t
  val get : 'a -> t
  val equal : t -> t -> bool
end

module TypeIdAllocator : sig
  (** @canonical Mlir.TypeIdAllocator *)

  class type t = object
    method allocate : TypeId.t
    method destroy : unit
    method raw : MlirTypeIDAllocator.t structure
  end

  val from_raw : MlirTypeIDAllocator.t structure -> t
  val get : unit -> t
end

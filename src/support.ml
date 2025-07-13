open Ctypes
open Bindings.Functions
open Error
open Utils

module LogicalResult = struct
  type t =
    | Success
    | Failure

  let from_raw raw =
    if mlir_logical_result_is_success raw
    then Success
    else if mlir_logical_result_is_failure raw
    then Failure
    else Error "Unknown logical result!" |> raise
end

module LLVMThreadPool = struct
  class t raw =
    object (self)
      method destroy = mlir_llvm_thread_pool_destroy self#raw
      method raw = raw
    end

  let from_raw = new t
  let get () = mlir_llvm_thread_pool_create () |> from_raw
end

module TypeId = struct
  class t raw =
    object (self)
      method hash = mlir_type_idhash_value self#raw
      method raw = raw
    end

  let from_raw = new t

  let get data =
    (* if alignment (ptr ctype) != 8
    then Error "TypeID pointer must be 8-byte aligned" |> raise *)
    Root.create data |> mlir_type_idcreate |> from_raw


  let equal t1 t2 = mlir_type_idequal t1#raw t2#raw
end

module TypeIdAllocator = struct
  class t raw =
    object (self)
      method allocate = mlir_type_idallocator_allocate_type_id self#raw |> TypeId.from_raw
      method destroy = mlir_type_idallocator_destroy self#raw
      method raw = raw
    end

  let from_raw = new t
  let get = mlir_type_idallocator_create >> from_raw
end

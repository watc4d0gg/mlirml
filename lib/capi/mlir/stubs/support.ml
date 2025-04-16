open Ctypes
open Types

module Bindings (F : Cstubs.FOREIGN) = struct
	open F

	(* StringRef *)
	let mlirStringRefCreate = foreign "mlirStringRefCreate" (string @-> int @-> returning (MlirStringRef.t))
	let mlirStringRefCreateFromCString = foreign "mlirStringRefCreateFromCString" (string @-> returning (MlirStringRef.t))
	let mlirStringRefEqual = foreign "mlirStringRefEqual" (MlirStringRef.t @-> MlirStringRef.t @-> returning (int))

	(* LogicalResult *)
	let mlirLogicalResultIsSuccess = foreign "mlirLogicalResultIsSuccess" (MlirLogicalResult.t @-> returning (int))
	let mlirLogicalResultIsFailure = foreign "mlirLogicalResultIsFailure" (MlirLogicalResult.t @-> returning (int))
	let mlirLogicalResultSuccess = foreign "mlirLogicalResultSuccess" (void @-> returning (MlirLogicalResult.t))
	let mlirLogicalResultFailure = foreign "mlirLogicalResultFailure" (void @-> returning (MlirLogicalResult.t))

	(* LLVMThreadPool *)
	let mlirLlvmThreadPoolCreate = foreign "mlirLlvmThreadPoolCreate" (void @-> returning (MlirLlvmThreadPool.t))
	let mlirLlvmThreadPoolDestroy = foreign "mlirLlvmThreadPoolDestroy" (MlirLlvmThreadPool.t @-> returning (void))

	(* TypeID *)
	let mlirTypeIDCreate = foreign "mlirTypeIDCreate" (ptr (void) @-> returning (MlirTypeID.t))
	let mlirTypeIDIsNull = foreign "mlirTypeIDIsNull" (MlirTypeID.t @-> returning (int))
	let mlirTypeIDEqual = foreign "mlirTypeIDEqual" (MlirTypeID.t @-> MlirTypeID.t @-> returning (int))
	let mlirTypeIDHashValue = foreign "mlirTypeIDHashValue" (MlirTypeID.t @-> returning (int))

	(* TypeIDAllocator *)
	let mlirTypeIDAllocatorCreate = foreign "mlirTypeIDAllocatorCreate" (void @-> returning (MlirTypeIDAllocator.t))
	let mlirTypeIDAllocatorDestroy = foreign "mlirTypeIDAllocatorDestroy" (MlirTypeIDAllocator.t @-> returning (void))
	let mlirTypeIDAllocatorAllocateTypeID = foreign "mlirTypeIDAllocatorAllocateTypeID" (MlirTypeIDAllocator.t @-> returning (MlirTypeID.t))

end
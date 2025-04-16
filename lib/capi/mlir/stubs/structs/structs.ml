open Ctypes

module Bindings (T : Cstubs_structs.TYPE) = struct
	open T

  (* Support.h *)
  module MlirLlvmThreadPool = struct
    type t
    let t : t structure typ = structure "MlirLlvmThreadPool"
  	let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

  module MlirTypeID = struct 
		type t
		let t : t structure typ = structure "MlirTypeID"
  	let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

  module MlirTypeIDAllocator = struct
		type t
		let t : t structure typ = structure "MlirTypeIDAllocator"
  	let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

  module MlirStringRef = struct
		type t
		let t : t structure typ = structure "MlirStringRef"
		let data = field t "data" (string)
		let length = field t "length" (int)
		let () = seal t
	end

  module MlirLogicalResult = struct
		type t
		let t : t structure typ = structure "MlirLogicalResult"
  	let value = field t "value" (int8_t)
		let () = seal t
	end


  (* IR.h *)
	type walkresult = Advance | Interrupt | Skip
	type walkorder = PreOrder | PostOrder

	let walkresult = enum "MlirWalkResult" ~typedef:true [
		Advance, constant "MlirWalkResultAdvance" int64_t;
		Interrupt, constant "MlirWalkResultInterrupt" int64_t;
		Skip, constant "MlirWalkResultSkip" int64_t;
	]

	let walkorder = enum "MlirWalkOrder" ~typedef:true [
		PreOrder, constant "MlirWalkPreOrder" int64_t;
		PostOrder, constant "MlirWalkPostOrder" int64_t;
	]

	module MlirAsmState = struct
		type t
		let t : t structure typ = structure "MlirAsmState"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirBytecodeWriterConfig = struct
		type t
		let t : t structure typ = structure "MlirBytecodeWriterConfig"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirContext = struct
		type t
		let t : t structure typ = structure "MlirContext"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirDialect = struct
		type t
		let t : t structure typ = structure "MlirDialect"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirDialectRegistry = struct
		type t
		let t : t structure typ = structure "MlirDialectRegistry"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirOperation = struct
		type t
		let t : t structure typ = structure "MlirOperation"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end
	
	module MlirOpOperand = struct
		type t
		let t : t structure typ = structure "MlirOpOperand"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirOpPrintingFlags = struct
		type t
		let t : t structure typ = structure "MlirOpPrintingFlags"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirBlock = struct 
		type t
		let t : t structure typ = structure "MlirBlock"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirRegion = struct 
		type t
		let t : t structure typ = structure "MlirRegion"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirSymbolTable = struct
		type t
		let t : t structure typ = structure "MlirSymbolTable"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirAttribute = struct 
		type t
		let t : t structure typ = structure "MlirAttribute"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirIdentifier = struct 
		type t
		let t : t structure typ = structure "MlirIdentifier"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirLocation = struct 
		type t
		let t : t structure typ = structure "MlirLocation"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirModule = struct
		type t
		let t : t structure typ = structure "MlirModule"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirType = struct 
		type t
		let t : t structure typ = structure "MlirType"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirValue = struct 
		type t
		let t : t structure typ = structure "MlirValue"
		let ptr = field t "ptr" (const (ptr (void)))
		let () = seal t
	end

	module MlirNamedAttribute = struct
		type t
		let t : t structure typ = structure "MlirNamedAttribute"
		let name = field t "name" (MlirIdentifier.t)
		let attribute = field t "attribute" (MlirAttribute.t)
		let () = seal t
	end

	module MlirDialectHandle = struct
		type t
		let t : t structure typ = structure "MlirDialectHandle"
		let ptr = field t "ptr" (ptr (void))
		let () = seal t
	end

	module MlirOperationState = struct
		type t
		let t : t structure typ = structure "MlirOperationState"
		let name = field t "name" (MlirStringRef.t)
		let location = field t "location" (MlirLocation.t)
		let nResults = field t "nResults" (intptr_t)
		let results = field t "results" (ptr (MlirType.t))
		let nOperands = field t "nOperands" (intptr_t)
		let operands = field t "operands" (ptr (MlirValue.t))
		let nRegions = field t "nRegions" (intptr_t)
		let regions = field t "regions" (ptr (MlirRegion.t))
		let nSuccessors = field t "nSuccessors" (intptr_t)
		let successors = field t "successors" (ptr (MlirBlock.t))
		let nAttributes = field t "nAttributes" (intptr_t)
		let attributes = field t "attributes" (ptr (MlirNamedAttribute.t))
		let enableResultTypeInference = field t "enableResultTypeInference" (int)
		let () = seal t
	end

end
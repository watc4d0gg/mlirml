open Ctypes

include Structs.Bindings (Mlir_structs_generated)

module MlirStringCallback = struct
  let t =  Foreign.funptr ~name:"MlirStringCallback" (MlirStringRef.t @-> (ptr (void)) @-> returning (void))
end

module MlirOperationWalkCallback = struct
  let t = Foreign.funptr ~name:"MlirOperationWalkCallback" (MlirOperation.t @-> (ptr (void)) @-> returning (walkresult))
end

module MlirCallback = struct
  let t = Foreign.funptr ~name:"callback" (MlirOperation.t @-> bool @-> (ptr (void)) @-> returning (void))
end
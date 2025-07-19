include Error
include Support
include Ir
include Affine_map.AffineMap
include Builtin_attributes
include Builtin_types
include Pass
include Arith
include Func
include Sparse_tensor
include Tensor
include Linalg

let print_as_string print_fn value =
  let out = ref String.empty in
  print_fn ~callback:(fun result -> out := !out ^ result) value;
  !out


let with_context ctx f = Fun.protect ~finally:(fun () -> ctx#destroy) (fun () -> f ctx)

let register_all_dialects registry =
  Bindings.Functions.mlir_register_all_dialects registry#raw


let register_all_llvm_translations ctx =
  Bindings.Functions.mlir_register_all_llvmtranslations ctx#raw


let register_all_passes = Bindings.Functions.mlir_register_all_passes

module Bindings = struct
  include Bindings.Types
end

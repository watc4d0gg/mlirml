open Ctypes
open Bindings.Types
open Bindings.Functions

type context = MlirContext.t structure

type mlir_module = MlirModule.t structure

type mlir_type = MlirType.t structure

type operation = MlirOperation.t structure

module Context = struct

	let create = mlir_context_create

	let equal = mlir_context_equal

	let destroy = mlir_context_destroy

end

module Module = struct

	let parse ctx data = mlir_module_create_parse ctx (mlir_string_ref_create_from_cstring data)

	let destroy = mlir_module_destroy

end

let with_context ctx f =
	let result = f ctx in
	Context.destroy ctx;
	result
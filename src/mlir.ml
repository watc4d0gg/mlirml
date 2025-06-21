include Arith
include Builtin_attributes
include Builtin_types
include Error
include Ir.Ir
include Support.Support
include Func
open Bindings.Functions

let print_as_string print_fn value =
  let out = ref String.empty in
  print_fn ~callback:(fun result -> out := !out ^ result) value;
  !out


let with_context (ctx : Context.t) f =
  Fun.protect ~finally:(fun () -> ctx#destroy) (fun () -> f ctx)


let register_all_dialects (registry : DialectRegistry.t) =
  mlir_register_all_dialects registry#raw


let register_all_llvm_translations (ctx : Context.t) =
  mlir_register_all_llvmtranslations ctx#raw


let register_all_passes = mlir_register_all_passes ()

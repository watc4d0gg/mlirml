open Mlir
open Mlir.Ir

let%test "ctx_equal" =
  with_context (Context.create None true) (fun ctx -> Context.equal ctx ctx) = true


let%test "all_dialects" =
  with_context (Context.create None true) (fun ctx ->
    let registry = DialectRegistry.create () in
    register_all_dialects registry;
    Context.append_dialect_registry ctx registry;
    Context.load_all_available_dialects ctx;
    let location = Location.unknown ctx in
    let _ = Module.create location in
    Location.equal location location)
  = true

open Mlir

let%test "ctx_equal" =
  with_context (Context.get None true) (fun ctx -> Context.equal ctx ctx) = true


let%test "arith" =
  with_context (Context.get None true) (fun ctx ->
    let registry = DialectRegistry.get () in
    register_all_dialects registry;
    ctx#append_dialect_registry registry;
    ctx#load_all_available_dialects;
    let location = Location.unknown ctx in
    let m = Module.empty location in
    let float_type = Type.float64 ctx in
    let one = Arith.constant (FloatAttr.get float_type 1.0 location) location ctx in
    m#body#append_operation one;
    let two = Arith.constant (FloatAttr.get float_type 2.0 location) location ctx in
    m#body#append_operation two;
    m#body#append_operation @@ Arith.subf (one#result 0) (two#result 0) location;
    let module_op = m#to_operation in
    assert module_op#verify;
    print_as_string Operation.print module_op)
  = "module {\n\
    \  %cst = arith.constant 1.000000e+00 : f64\n\
    \  %cst_0 = arith.constant 2.000000e+00 : f64\n\
    \  %0 = arith.subf %cst, %cst_0 : f64\n\
     }\n"


let%test "func" =
  with_context (Context.get None true) (fun ctx ->
    let registry = DialectRegistry.get () in
    register_all_dialects registry;
    ctx#append_dialect_registry registry;
    ctx#load_all_available_dialects;
    let location = Location.unknown ctx in
    let index_type = Type.index ctx in
    let m = Module.empty location in
    let block = Block.get [ index_type, location; index_type, location ] in
    let sum = Arith.addi (block#argument 0) (block#argument 1) location in
    block#append_operation sum;
    block#append_operation @@ Func.return [ sum#result 0 ] location;
    let region = Region.get () in
    region#append_block block;
    m#body#append_operation
    @@ Func.func
         ctx
         (StringAttr.get ctx "add")
         (Function.get ctx [ index_type; index_type ] [ index_type ] |> TypedAttr.get)
         region
         []
         location;
    let module_op = m#to_operation in
    assert module_op#verify;
    print_as_string Operation.print module_op)
  = "module {\n\
    \  func.func @add(%arg0: index, %arg1: index) -> index {\n\
    \    %0 = arith.addi %arg0, %arg1 : index\n\
    \    return %0 : index\n\
    \  }\n\
     }\n"

(* let%test "matmul" =
  with_context (Context.get None true) (fun ctx ->
    let registry = DialectRegistry.get () in
    register_all_dialects registry;
    ctx#append_dialect_registry registry;
    ctx#load_all_available_dialects;
    let location = Location.unknown ctx in
    ) *)

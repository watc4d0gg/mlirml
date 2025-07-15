open Mlir.Ir
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
    m#body#append_operation
    @@ Func.func
         ctx
         (StringAttr.get ctx "add")
         (FunctionType.get ctx [ index_type; index_type ] [ index_type ] |> TypedAttr.get)
         []
         location
         ~init:(fun body ->
           let sum = Arith.addi (body#argument 0) (body#argument 1) location in
           body#append_operation sum;
           body#append_operation @@ Func.return [ sum#result 0 ] location);
    let module_op = m#to_operation in
    assert module_op#verify;
    print_as_string Operation.print module_op)
  = "module {\n\
    \  func.func @add(%arg0: index, %arg1: index) -> index {\n\
    \    %0 = arith.addi %arg0, %arg1 : index\n\
    \    return %0 : index\n\
    \  }\n\
     }\n"


let%test "matmul" =
  with_context (Context.get None true) (fun ctx ->
    let registry = DialectRegistry.get () in
    register_all_dialects registry;
    ctx#append_dialect_registry registry;
    ctx#load_all_available_dialects;
    let location = Location.unknown ctx in
    let float64 = Type.float64 ctx in
    let indexing_maps =
      [ AffineMap.get ctx 3 0 [AffineExpr.dim ctx 0; AffineExpr.dim ctx 1]
      ; AffineMap.get ctx 3 0 [AffineExpr.dim ctx 1; AffineExpr.dim ctx 2]
      ; AffineMap.get ctx 3 0 [AffineExpr.dim ctx 0; AffineExpr.dim ctx 2]
      ]
    in
    let iterator_types = [ "parallel"; "reduction"; "parallel"] in
    let dcsr =
      SparseTensorEncodingAttr.get
        ctx
        [ LevelType.compressed []
        ; LevelType.compressed []
        ]
        (AffineMap.multi_dim_identity ctx 2)
        None
        None
        None
        None
        None
    in
    let index_type = Type.index ctx in
    let tensor_type =
      RankedTensorType.get
        [ ShapedType.Dynamic; ShapedType.Dynamic ]
        float64
        (Some dcsr)
        location
    in
    let m = Module.empty location in
    m#body#append_operation
    @@ Func.func
         ctx
         (StringAttr.get ctx "matmul")
         (FunctionType.get ctx [ tensor_type; tensor_type ] [ tensor_type ]
          |> TypedAttr.get)
         []
         location
         ~init:(fun body ->
           let c0 = Arith.constant (IntegerAttr.get index_type 0) location ctx in
           body#append_operation c0;
           let c1 = Arith.constant (IntegerAttr.get index_type 1) location ctx in
           body#append_operation c1;
           let empty = Tensor.empty [ c0#result 0; c1#result 0 ] tensor_type location in
           body#append_operation empty;
           let kernel =
             Linalg.generic
               ctx
               [ body#argument 0; body#argument 1 ]
               [ empty#result 0 ]
               indexing_maps
               iterator_types
               location
               ~init:(fun body ->
                 let mul = Arith.mulf (body#argument 0) (body#argument 1) location in
                 body#append_operation mul;
                 let sum = Arith.addf (mul#result 0) (body#argument 2) location in
                 body#append_operation sum;
                 body#append_operation @@ Linalg.yield [ sum#result 0 ] location)
           in
           body#append_operation kernel;
           body#append_operation @@ Func.return [ kernel#result 0 ] location);
    let module_op = m#to_operation in
    assert module_op#verify;
    print_as_string Operation.print module_op)
  = "#map = affine_map<(d0, d1, d2) -> (d0, d1)>\n\
     #map1 = affine_map<(d0, d1, d2) -> (d1, d2)>\n\
     #map2 = affine_map<(d0, d1, d2) -> (d0, d2)>\n\
     #sparse = #sparse_tensor.encoding<{ map = (d0, d1) -> (d0 : compressed, d1 : \
     compressed) }>\n\
     module {\n\
    \  func.func @matmul(%arg0: tensor<?x?xf64, #sparse>, %arg1: tensor<?x?xf64, \
     #sparse>) -> tensor<?x?xf64, #sparse> {\n\
    \    %c0 = arith.constant 0 : index\n\
    \    %c1 = arith.constant 1 : index\n\
    \    %0 = tensor.empty(%c0, %c1) : tensor<?x?xf64, #sparse>\n\
    \    %1 = linalg.generic {indexing_maps = [#map, #map1, #map2], iterator_types = \
     [\"parallel\", \"reduction\", \"parallel\"]} ins(%arg0, %arg1 : tensor<?x?xf64, #sparse>, \
     tensor<?x?xf64, #sparse>) outs(%0 : tensor<?x?xf64, #sparse>) {\n\
    \    ^bb0(%in: f64, %in_0: f64, %out: f64):\n\
    \      %2 = arith.mulf %in, %in_0 : f64\n\
    \      %3 = arith.addf %2, %out : f64\n\
    \      linalg.yield %3 : f64\n\
    \    } -> tensor<?x?xf64, #sparse>\n\
    \    return %1 : tensor<?x?xf64, #sparse>\n\
    \  }\n\
     }\n"

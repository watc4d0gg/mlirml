open Ir.Ir
open Builtin_attributes
open Builtin_types

module Linalg = struct
  let generic ctx inputs outputs indexing_map iterator_types ~init location =
    let operandSegmentSizes =
      [ List.length inputs |> Int32.of_int; List.length outputs |> Int32.of_int ]
    in
    OpBuilder.get "linalg.generic" location
    |> OpBuilder.add_operands inputs
    |> OpBuilder.add_operands outputs
    |> OpBuilder.add_attributes
         [ ( Identifier.get ctx "operationSegmentSizes"
           , DenseInt32ArrayAttr.get ctx operandSegmentSizes )
         ]
    |> OpBuilder.add_attributes
         [ ( Identifier.get ctx "indexing_map"
           , List.map AffineMapAttr.get indexing_map |> ArrayAttr.get ctx )
         ]
    |> OpBuilder.add_attributes
         [ ( Identifier.get ctx "iterator_types"
           , List.map (StringAttr.get ctx) iterator_types |> ArrayAttr.get ctx )
         ]
    |> OpBuilder.add_regions
         [ (let region = Region.get () in
            let input_types =
              List.map
                (fun value ->
                   let t = value#get_type in
                   if Type.is_shaped t then (Shaped.cast t)#element_type else t)
                inputs
            in
            let output_types =
              List.map
                (fun value ->
                   let t = value#get_type in
                   if Type.is_shaped t then (Shaped.cast t)#element_type else t)
                outputs
            in
            let arg_types = List.append input_types output_types in
            let locs =
              List.init (List.length arg_types) (fun _ -> location)
            in
            let block = Block.get (List.combine arg_types locs) in
            init block;
            region#append_block block;
            region)
         ]
    |> OpBuilder.enable_result_type_inference
    |> OpBuilder.build true
end

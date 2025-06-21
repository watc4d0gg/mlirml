open Ir.Ir

let _unary_operation name operand attributes location =
  OpBuilder.get name location
  |> OpBuilder.add_operands [ operand ]
  |> OpBuilder.add_attributes attributes
  |> OpBuilder.enable_result_type_inference
  |> OpBuilder.build true


let binary_operation name lhs rhs attributes location =
  OpBuilder.get name location
  |> OpBuilder.add_operands [ lhs ]
  |> OpBuilder.add_operands [ rhs ]
  |> OpBuilder.add_attributes attributes
  |> OpBuilder.enable_result_type_inference
  |> OpBuilder.build true


module Arith = struct
  let addi lhs rhs location = binary_operation "arith.addi" lhs rhs [] location
  let addf lhs rhs location = binary_operation "arith.addf" lhs rhs [] location
  let subf lhs rhs location = binary_operation "arith.subf" lhs rhs [] location
  let mulf lhs rhs location = binary_operation "arith.mulf" lhs rhs [] location

  let constant value location ctx =
    OpBuilder.get "arith.constant" location
    |> OpBuilder.add_attributes [ Identifier.get ctx "value", value ]
    |> OpBuilder.enable_result_type_inference
    |> OpBuilder.build true
end

open Ir.Ir

module Tensor = struct
  let empty dynamic_sizes result_type location =
    OpBuilder.get "tensor.empty" location
    |> OpBuilder.add_operands dynamic_sizes
    |> OpBuilder.add_results [ result_type ]
    |> OpBuilder.build true

  let insert value destination indices location =
    OpBuilder.get "tensor.insert" location
    |> OpBuilder.add_operands [ value ]
    |> OpBuilder.add_operands [ destination ]
    |> OpBuilder.add_operands indices
    |> OpBuilder.add_results [ destination#get_type ]
    |> OpBuilder.build true
end

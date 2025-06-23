open Ir.Ir

module Tensor = struct
  let empty dynamic_sizes result_type location =
    OpBuilder.get "tensor.empty" location
    |> OpBuilder.add_operands dynamic_sizes
    |> OpBuilder.add_results [ result_type ]
    |> OpBuilder.build true
end

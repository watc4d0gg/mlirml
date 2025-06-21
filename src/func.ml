open Ir.Ir
open Builtin_attributes

module Func = struct
  let func ctx (name : StringAttr.t) (t : #TypedAttr.t) body attributes location =
    OpBuilder.get "func.func" location
    |> OpBuilder.add_attributes [ Identifier.get ctx "sym_name", name ]
    |> OpBuilder.add_attributes [ Identifier.get ctx "function_type", t ]
    |> OpBuilder.add_attributes attributes
    |> OpBuilder.add_regions [ body ]
    |> OpBuilder.build true


  let return results location =
    OpBuilder.get "func.return" location
    |> OpBuilder.add_operands results
    |> OpBuilder.build false
end

open Ir.Ir
open Builtin_attributes
open Builtin_types

module Func = struct

  let return results location =
    OpBuilder.get "func.return" location
    |> OpBuilder.add_operands results
    |> OpBuilder.build false
  
  let func ctx (name : StringAttr.t) (t : #TypedAttr.t) attributes ~init location =
    OpBuilder.get "func.func" location
    |> OpBuilder.add_attributes [ Identifier.get ctx "sym_name", name ]
    |> OpBuilder.add_attributes [ Identifier.get ctx "function_type", t ]
    |> OpBuilder.add_attributes attributes
    |> OpBuilder.add_regions
         [ (let region = Region.get () in
            assert (Type.is_function t#value);
            let inputs =
              (FunctionType.cast t#value)#map_results ~f:(fun input_type ->
                input_type, location)
            in
            let body = Block.get inputs in
            let results = init body in
            body#append_operation @@ return results location;
            region#append_block body;
            region)
         ]
    |> OpBuilder.build true
end

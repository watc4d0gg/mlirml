open Ctypes
open Bindings.Types
open Bindings.Functions
open Ir.Ir


module Rewriter = struct
  class t raw = object (self)
    method context = mlir_rewriter_base_get_context self#raw
    method clear_insertion_point = mlir_rewriter_base_clear_insertion_point self#raw
    method set_insertion_point_before (op : Operation.t) = mlir_rewriter_base_set_insertion_point_before self#raw op#raw
    method set_insertion_point_after (op : Operation.t) = mlir_rewriter_base_set_insertion_point_after self#raw op#raw
    method set_insertion_point_after_value (value : Value.t) = mlir_rewriter_base_set_insertion_point_after_value self#raw value#raw
    method set_insertion_point_to_start (block : Block.t) = mlir_rewriter_base_set_insertion_point_to_start self#raw block#raw
    method set_insertion_point_to_end (block : Block.t) = mlir_rewriter_base_set_insertion_point_to_end self#raw block#raw
    method insertion_block = mlir_rewriter_base_get_insertion_block self#raw |> Block.from_raw
    method block = mlir_rewriter_base_get_block self#raw |> Block.from_raw
    method insert (op : Operation.t) = mlir_rewriter_base_insert self#raw op#raw |> Operation.from_raw
    method clone (op : Operation.t) = mlir_rewriter_base_clone self#raw op#raw |> Operation.from_raw
    method clone_without_regions (op : Operation.t) = mlir_rewriter_base_clone_without_regions self#raw op#raw |> Operation.from_raw
    method clone_region_before (region : Region.t) (block : Block.t) = mlir_rewriter_base_clone_region_before self#raw region#raw block#raw
    method inline_region_before (region : Region.t) (block : Block.t) = mlir_rewriter_base_inline_region_before self#raw region#raw block#raw
    method replace_op_with_values : 'a. Operation.t -> (#Value.t as 'a) list -> unit = fun op values -> 
      let value_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) values) in
      mlir_rewriter_base_replace_op_with_values self#raw op#raw (List.length values |> Intptr.of_int) (CArray.start value_array)
    method replace_op_with_operation (op : Operation.t) (newOp : Operation.t) = mlir_rewriter_base_replace_op_with_operation self#raw op#raw newOp#raw
    method erase_op (op : Operation.t) = mlir_rewriter_base_erase_op self#raw op#raw
    method erase_block (block : Block.t) = mlir_rewriter_base_erase_block self#raw block#raw
    method inline_block_before : 'a. Block.t -> Operation.t -> (#Value.t as 'a) list -> unit = fun source op values ->
      let value_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) values) in
      mlir_rewriter_base_inline_block_before self#raw source#raw op#raw (List.length values |> Intptr.of_int) (CArray.start value_array)
    method merge_blocks : 'a. Block.t -> Block.t -> (#Value.t as 'a) list -> unit = fun source dest values ->
      let value_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) values) in
      mlir_rewriter_base_merge_blocks self#raw source#raw dest#raw (List.length values |> Intptr.of_int) (CArray.start value_array)
    method move_op_before (op : Operation.t) (newOp : Operation.t) = mlir_rewriter_base_move_op_before self#raw op#raw newOp#raw
    method move_op_after (op : Operation.t) (newOp : Operation.t) = mlir_rewriter_base_move_op_after self#raw op#raw newOp#raw
    method move_block_after (block : Block.t) (existing_block : Block.t) = mlir_rewriter_base_move_block_before self#raw block#raw existing_block#raw
    method start_op_modification (op : Operation.t) = mlir_rewriter_base_start_op_modification self#raw op#raw
    method finalize_op_modification (op : Operation.t) = mlir_rewriter_base_finalize_op_modification self#raw op#raw
    method cancel_op_modification (op : Operation.t) = mlir_rewriter_base_cancel_op_modification self#raw op#raw
    method replace_all_uses_with : 'a 'b. (#Value.t as 'a) -> (#Value.t as 'b) -> unit = fun from to_ -> 
      mlir_rewriter_base_replace_all_uses_with self#raw from#raw to_#raw
    method replace_all_value_range_uses_with : 'a 'b. (#Value.t as 'a) list -> (#Value.t as 'b) list -> unit = fun from to_ ->
      assert (List.length from = List.length to_);
      let from_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) from)
      and to_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) to_) in 
      mlir_rewriter_base_replace_all_value_range_uses_with self#raw (List.length from |> Intptr.of_int) (CArray.start from_array) (CArray.start to_array)
    method replace_all_op_uses_with_value_range : 'a. Operation.t -> (#Value.t as 'a) list -> unit = fun from to_ ->
      let to_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) to_) in
      mlir_rewriter_base_replace_all_op_uses_with_value_range self#raw from#raw (List.length to_ |> Intptr.of_int) (CArray.start to_array)
    method replace_all_op_uses_with_operation (from : Operation.t) (to_ : Operation.t) = mlir_rewriter_base_replace_all_op_uses_with_operation self#raw from#raw to_#raw
    method replace_op_uses_within_block : 'a. Operation.t -> (#Value.t as 'a) list -> Block.t -> unit = fun op newValues block ->
      let value_array = CArray.of_list MlirValue.t (List.map (fun value -> value#raw) newValues) in
       mlir_rewriter_base_replace_op_uses_within_block self#raw op#raw (List.length newValues |> Intptr.of_int) (CArray.start value_array) block#raw
    method replace_all_uses_except (from : Value.t) (to_ : Value.t) (excepted_user : Operation.t) =
      mlir_rewriter_base_replace_all_uses_except self#raw from#raw to_#raw excepted_user#raw
    method destroy = mlir_irrewriter_destroy self#raw
    method raw = raw
  end

  let from_raw = new t

  let get ctx = mlir_irrewriter_create ctx#raw |> from_raw

  let get_from_op op = mlir_irrewriter_create_from_op op#raw |> from_raw
end
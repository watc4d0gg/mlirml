open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Ir.Ir
open Utils

module AffineMap = struct

  module AffineExpr = struct
    type raw = MlirAffineExpr.t structure

    type t =
      | Dim of int * raw
      | Symbol of int * raw
      | Constant of int * raw
      | Add of t * t * raw
      | Mul of t * t * raw
      | Mod of t * t * raw
      | CeilDiv of t * t * raw
      | FloorDiv of t * t * raw

    let rec from_raw raw =
      if mlir_affine_expr_is_adim raw then Dim (mlir_affine_dim_expr_get_position raw |> Intptr.to_int, raw)
      else if mlir_affine_expr_is_asymbol raw then Symbol (mlir_affine_symbol_expr_get_position raw |> Intptr.to_int, raw)
      else if mlir_affine_expr_is_aconstant raw then Constant (mlir_affine_constant_expr_get_value raw |> Int64.to_int, raw)
      else if mlir_affine_expr_is_aadd raw then Add (mlir_affine_binary_op_expr_get_lhs raw |> from_raw, mlir_affine_binary_op_expr_get_rhs raw |> from_raw, raw)
      else if mlir_affine_expr_is_amul raw then Mul (mlir_affine_binary_op_expr_get_lhs raw |> from_raw, mlir_affine_binary_op_expr_get_rhs raw |> from_raw, raw)
      else if mlir_affine_expr_is_amod raw then Mod (mlir_affine_binary_op_expr_get_lhs raw |> from_raw, mlir_affine_binary_op_expr_get_rhs raw |> from_raw, raw)
      else if mlir_affine_expr_is_aceil_div raw then CeilDiv (mlir_affine_binary_op_expr_get_lhs raw |> from_raw, mlir_affine_binary_op_expr_get_rhs raw |> from_raw, raw)
      else if mlir_affine_expr_is_afloor_div raw then FloorDiv (mlir_affine_binary_op_expr_get_lhs raw |> from_raw, mlir_affine_binary_op_expr_get_rhs raw |> from_raw, raw)
      else Error ("Unable to construct an affine expression from " ^ print_raw_as_string mlir_affine_expr_print raw) |> raise

    let raw = function
    | Dim (_, raw) -> raw
    | Symbol (_, raw) -> raw
    | Constant (_, raw) -> raw
    | Add (_, _, raw) -> raw
    | Mul (_, _, raw) -> raw
    | Mod (_, _, raw) -> raw
    | CeilDiv (_, _, raw) -> raw
    | FloorDiv (_, _, raw) -> raw

    let dim ctx pos = mlir_affine_dim_expr_get ctx#raw (Intptr.of_int pos) |> from_raw

    let symbol ctx pos = mlir_affine_symbol_expr_get ctx#raw (Intptr.of_int pos) |> from_raw

    let constant ctx value = mlir_affine_constant_expr_get ctx#raw (Int64.of_int value) |> from_raw

    let add lhs rhs = mlir_affine_mul_expr_get (raw lhs) (raw rhs) |> from_raw

    let mul lhs rhs = mlir_affine_mul_expr_get (raw lhs) (raw rhs) |> from_raw

    let modulo lhs rhs = mlir_affine_mod_expr_get (raw lhs) (raw rhs) |> from_raw

    let ceil_div lhs rhs = mlir_affine_ceil_div_expr_get (raw lhs) (raw rhs) |> from_raw

    let floor_div lhs rhs = mlir_affine_floor_div_expr_get (raw lhs) (raw rhs) |> from_raw

    let context expr = mlir_affine_expr_get_context (raw expr) |> Context.from_raw

    let compose expr map =
      mlir_affine_expr_compose (raw expr) map#raw |> from_raw
    
    let is_symbolic_or_constant expr = raw expr |> mlir_affine_expr_is_symbolic_or_constant
    
    let is_pure_affine expr = raw expr |> mlir_affine_expr_is_symbolic_or_constant

    let largest_known_divisor expr = raw expr |> mlir_affine_expr_get_largest_known_divisor |> Int64.to_int

    let is_multiple_of expr factor = mlir_affine_expr_is_multiple_of (raw expr) (Int64.of_int factor)

    let is_function_of_dim expr position =
      mlir_affine_expr_is_function_of_dim (raw expr) (Intptr.of_int position)

    let is_binary expr = raw expr |> mlir_affine_expr_is_abinary
    let equal e1 e2 = mlir_affine_expr_equal (raw e1) (raw e2)
    let print ~callback expr = print_raw mlir_affine_expr_print ~callback (raw expr)
    let dump expr = raw expr |> mlir_affine_expr_dump
  end

  module AffineMap = struct

    class t raw = object (self)
      method context = mlir_affine_map_get_context self#raw |> Context.from_raw
      method is_identity = mlir_affine_map_is_identity self#raw
      method is_minor_identity = mlir_affine_map_is_minor_identity self#raw
      method is_empty = mlir_affine_map_is_empty self#raw
      method is_constant = mlir_affine_map_is_single_constant self#raw
      method is_projected_permutation = mlir_affine_map_is_projected_permutation self#raw
      method is_permutation = mlir_affine_map_is_permutation self#raw

      method constant_result =
        if self#is_constant
        then Some (mlir_affine_map_get_single_constant_result self#raw |> Int64.to_int)
        else None

      method dims = mlir_affine_map_get_num_dims self#raw |> Intptr.to_int
      method symbols = mlir_affine_map_get_num_symbols self#raw |> Intptr.to_int
      method results = mlir_affine_map_get_num_results self#raw |> Intptr.to_int

      method result position =
        mlir_affine_map_get_result self#raw (Intptr.of_int position) |> AffineExpr.from_raw

      method inputs = mlir_affine_map_get_num_inputs self#raw |> Intptr.to_int

      method sub_map result_positions =
        let pos_array =
          CArray.of_list Ctypes.intptr_t (List.map Intptr.of_int result_positions)
        in
        mlir_affine_map_get_sub_map
          self#raw
          (List.length result_positions |> Intptr.of_int)
          (CArray.start pos_array)
        |> new t

      method major_sub_map results =
        mlir_affine_map_get_major_sub_map self#raw (Intptr.of_int results) |> new t

      method minor_sub_map results =
        mlir_affine_map_get_minor_sub_map self#raw (Intptr.of_int results) |> new t

      method replace expr replacement newDims newSymbols =
        mlir_affine_map_replace
          self#raw
          (AffineExpr.raw expr)
          (AffineExpr.raw replacement)
          (Intptr.of_int newDims)
          (Intptr.of_int newSymbols)
        |> new t

      method raw = raw
    end

    let from_raw = new t
    let empty ctx = mlir_affine_map_empty_get ctx#raw |> from_raw

    let zero ctx dims symbols =
      mlir_affine_map_zero_result_get ctx#raw (Intptr.of_int dims) (Intptr.of_int symbols)
      |> from_raw


    let get ctx dims symbols exprs =
      let expr_array =
        CArray.of_list MlirAffineExpr.t (List.map AffineExpr.raw exprs)
      in
      mlir_affine_map_get
        ctx#raw
        (Intptr.of_int dims)
        (Intptr.of_int symbols)
        (List.length exprs |> Intptr.of_int)
        (CArray.start expr_array)
      |> from_raw


    let constant ctx value =
      mlir_affine_map_constant_get ctx#raw (Int64.of_int value) |> from_raw


    let multi_dim_identity ctx dims =
      mlir_affine_map_multi_dim_identity_get ctx#raw (Intptr.of_int dims) |> from_raw


    let minor_identity ctx dims results =
      mlir_affine_map_minor_identity_get
        ctx#raw
        (Intptr.of_int dims)
        (Intptr.of_int results)
      |> from_raw


    let permutation ctx permutation =
      let perm_array =
        CArray.of_list Ctypes.uint (List.map Unsigned.UInt.of_int permutation)
      in
      mlir_affine_map_permutation_get
        ctx#raw
        (List.length permutation |> Intptr.of_int)
        (CArray.start perm_array)
      |> from_raw


    (**
    Returns the simplified affine map resulting from dropping the symbols that
    do not appear in any of the individual maps in `affineMaps`.
    Asserts that all maps in `affineMaps` are normalized to the same number of
    dims and symbols.
    *)
    let compress_unused_symbols maps =
      let result = ref []
      and map_array =
        CArray.of_list MlirAffineMap.t (List.map (fun map -> map#raw) maps)
      in
      let funptr =
        coerce
          (Foreign.funptr (ptr void @-> intptr_t @-> MlirAffineMap.t @-> returning void))
          (static_funptr (ptr void @-> intptr_t @-> MlirAffineMap.t @-> returning void))
          (fun _ _ m -> result := m :: !result)
      in
      mlir_affine_map_compress_unused_symbols
        (CArray.start map_array)
        (List.length maps |> Intptr.of_int)
        null
        funptr;
      List.rev_map from_raw !result


    let equal m1 m2 = mlir_affine_map_equal m1#raw m2#raw

    (**
    Prints an affine map by sending chunks of the string representation to `callback`.
    Note that the callback may be called several times
    with consecutive chunks of the string.
    *)
    let print ~callback map = print_raw mlir_affine_map_print ~callback map#raw

    let dump map = mlir_affine_map_dump map#raw
  end
end

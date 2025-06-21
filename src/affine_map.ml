open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Ir.Ir
open Utils

class affine_expr raw =
  object (self)
    method context = mlir_affine_expr_get_context self#raw |> Context.from_raw

    method compose (map : affine_map) =
      mlir_affine_expr_compose self#raw map#raw |> new affine_expr

    method raw = raw
  end

and affine_map raw =
  object (self)
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
      mlir_affine_map_get_result self#raw (Intptr.of_int position) |> new affine_expr

    method inputs = mlir_affine_map_get_num_inputs self#raw |> Intptr.to_int

    method sub_map result_positions =
      let pos_array =
        CArray.of_list Ctypes.intptr_t (List.map Intptr.of_int result_positions)
      in
      mlir_affine_map_get_sub_map
        self#raw
        (List.length result_positions |> Intptr.of_int)
        (CArray.start pos_array)
      |> new affine_map

    method major_sub_map results =
      mlir_affine_map_get_major_sub_map self#raw (Intptr.of_int results) |> new affine_map

    method minor_sub_map results =
      mlir_affine_map_get_minor_sub_map self#raw (Intptr.of_int results) |> new affine_map

    method replace
      :  'a 'b.
         (< raw : MlirAffineExpr.t structure
          ; context : Context.t
          ; compose : affine_map -> affine_expr
          ; .. >
          as
          'a)
      -> (< raw : MlirAffineExpr.t structure
          ; context : Context.t
          ; compose : affine_map -> affine_expr
          ; .. >
          as
          'b)
      -> int
      -> int
      -> affine_map =
      fun expr replacement newDims newSymbols ->
        mlir_affine_map_replace
          self#raw
          expr#raw
          replacement#raw
          (Intptr.of_int newDims)
          (Intptr.of_int newSymbols)
        |> new affine_map

    method raw = raw
  end

module AffineMap = struct
  module AffineMap = struct
    class t = affine_map

    let from_raw = new t
    let empty ctx = mlir_affine_map_empty_get ctx#raw |> from_raw

    let zero ctx dims symbols =
      mlir_affine_map_zero_result_get ctx#raw (Intptr.of_int dims) (Intptr.of_int symbols)
      |> from_raw


    let get ctx dims symbols exprs =
      let expr_array =
        CArray.of_list MlirAffineExpr.t (List.map (fun expr -> expr#raw) exprs)
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

  module AffineExpr = struct
    class t = affine_expr

    let from_raw = new t
    let is_symbolic_or_constant expr = mlir_affine_expr_is_symbolic_or_constant expr#raw
    let is_pure_affine expr = mlir_affine_expr_is_pure_affine expr#raw

    let largest_known_divisor expr =
      mlir_affine_expr_get_largest_known_divisor expr#raw |> Int64.to_int


    let is_multiple_of expr factor =
      mlir_affine_expr_is_multiple_of expr#raw (Int64.of_int factor)


    let is_function_of_dim expr position =
      mlir_affine_expr_is_function_of_dim expr#raw (Intptr.of_int position)


    let is_binary expr = mlir_affine_expr_is_abinary expr#raw
    let equal e1 e2 = mlir_affine_expr_equal e1#raw e2#raw
    let print ~callback expr = print_raw mlir_affine_expr_print ~callback expr#raw
    let dump expr = mlir_affine_expr_dump expr#raw
  end

  module AffineDimExpr = struct
    class t raw =
      object (self)
        initializer
          if mlir_affine_expr_is_adim raw
          then ()
          else
            Error
              ("Unable to cast the affine expression "
               ^ print_raw_as_string mlir_affine_expr_print raw
               ^ " to a AffineDimExpr")
            |> raise

        inherit AffineExpr.t raw
        method position = mlir_affine_dim_expr_get_position self#raw |> Intptr.to_int
      end

    let from_raw = new t

    let get ctx position =
      mlir_affine_dim_expr_get ctx#raw (Intptr.of_int position) |> from_raw
  end

  module AffineSymbolExpr = struct
    class t raw =
      object (self)
        initializer
          if mlir_affine_expr_is_asymbol raw
          then ()
          else
            Error
              ("Unable to cast the affine expression "
               ^ print_raw_as_string mlir_affine_expr_print raw
               ^ " to a AffineSymbolExpr")
            |> raise

        inherit AffineExpr.t raw
        method position = mlir_affine_symbol_expr_get_position self#raw |> Intptr.to_int
      end

    let from_raw = new t

    let get ctx position =
      mlir_affine_symbol_expr_get ctx#raw (Intptr.of_int position) |> from_raw
  end

  module AffineConstantExpression = struct
    class t raw =
      object (self)
        initializer
          if mlir_affine_expr_is_asymbol raw
          then ()
          else
            Error
              ("Unable to cast the affine expression "
               ^ print_raw_as_string mlir_affine_expr_print raw
               ^ " to a AffineConstantExpr")
            |> raise

        inherit AffineExpr.t raw
        method value = mlir_affine_constant_expr_get_value self#raw |> Int64.to_int
      end

    let from_raw = new t

    let get ctx value =
      mlir_affine_constant_expr_get ctx#raw (Int64.of_int value) |> from_raw
  end

  class affine_binary_expr type_name init_check raw =
    object (self)
      initializer
        if init_check raw
        then ()
        else
          Error
            ("Unable to cast the affine expression "
             ^ print_raw_as_string mlir_affine_expr_print raw
             ^ " to a "
             ^ type_name)
          |> raise

      inherit AffineExpr.t raw
      method lhs = mlir_affine_binary_op_expr_get_lhs self#raw |> AffineExpr.from_raw
      method rhs = mlir_affine_binary_op_expr_get_rhs self#raw |> AffineExpr.from_raw
    end

  module AffineAddExpression = struct
    class t = affine_binary_expr "AffineAddExpr" mlir_affine_expr_is_aadd

    let from_raw = new t
    let get lhs rhs = mlir_affine_add_expr_get lhs#raw rhs#raw |> from_raw
  end

  module AffineMulExpression = struct
    class t = affine_binary_expr "AffineMulExpr" mlir_affine_expr_is_amul

    let from_raw = new t
    let get lhs rhs = mlir_affine_mul_expr_get lhs#raw rhs#raw |> from_raw
  end

  module AffineModExpression = struct
    class t = affine_binary_expr "AffineModExpr" mlir_affine_expr_is_amod

    let from_raw = new t
    let get lhs rhs = mlir_affine_mod_expr_get lhs#raw rhs#raw |> from_raw
  end

  module AffineFloorDivExpression = struct
    class t = affine_binary_expr "AffineFloorDivExpr" mlir_affine_expr_is_afloor_div

    let from_raw = new t
    let get lhs rhs = mlir_affine_floor_div_expr_get lhs#raw rhs#raw |> from_raw
  end

  module AffineCeilDivExpression = struct
    class t = affine_binary_expr "AffineCeilDivExpr" mlir_affine_expr_is_aceil_div

    let from_raw = new t
    let get lhs rhs = mlir_affine_ceil_div_expr_get lhs#raw rhs#raw |> from_raw
  end
end

open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Ir.Ir
open Support
open Affine_map.AffineMap
open Utils

module SparseTensorFormat = struct
  type t =
    | Dense
    | Batch
    | Compressed
    | Singleton
    | LooseCompressed
    | NOutOfM
end

module SparseTensorProperty = struct
  type t =
    | NonUnique
    | NonOrdered
end

module SparseTensorLevelType = struct
  type t = Unsigned.uint64

  let build fmt properties n_opt m_opt =
    let prop_array =
      CArray.of_list
        MlirSparseTensorLevelPropertyNondefault.t
        (List.map
           (fun prop ->
              match prop with
              | SparseTensorProperty.NonUnique ->
                MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_NON_UNIQUE
              | SparseTensorProperty.NonOrdered ->
                MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_NON_ORDERED)
           properties)
    in
    mlir_sparse_tensor_encoding_attr_build_lvl_type
      (match fmt with
       | SparseTensorFormat.Dense ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_DENSE
       | SparseTensorFormat.Batch ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_BATCH
       | SparseTensorFormat.Compressed ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_COMPRESSED
       | SparseTensorFormat.Singleton ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_SINGLETON
       | SparseTensorFormat.LooseCompressed ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_LOOSE_COMPRESSED
       | SparseTensorFormat.NOutOfM ->
         MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_N_OUT_OF_M)
      (CArray.start prop_array)
      (List.length properties |> Unsigned.UInt.of_int)
      (Option.value n_opt ~default:0 |> Unsigned.UInt.of_int)
      (Option.value m_opt ~default:0 |> Unsigned.UInt.of_int)


  let structured_n lvl_type =
    let value = mlir_sparse_tensor_encoding_attr_get_structured_n lvl_type in
    if Unsigned.UInt.equal Unsigned.UInt.zero value
    then None
    else Some (Unsigned.UInt.to_int value)


  let structured_m lvl_type =
    let value = mlir_sparse_tensor_encoding_attr_get_structured_m lvl_type in
    if Unsigned.UInt.equal Unsigned.UInt.zero value
    then None
    else Some (Unsigned.UInt.to_int value)
end

module SparseTensorEncodingAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_asparse_tensor_encoding_attr raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a SparseTensorEncodingAttr")
          |> raise

      method level_rank =
        mlir_sparse_tensor_encoding_get_lvl_rank self#raw |> Intptr.to_int

      method level_type lvl =
        mlir_sparse_tensor_encoding_attr_get_lvl_type self#raw (Intptr.of_int lvl)

      method level_format lvl =
        match
          mlir_sparse_tensor_encoding_attr_get_lvl_fmt self#raw (Intptr.of_int lvl)
        with
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_DENSE ->
          SparseTensorFormat.Dense
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_BATCH ->
          SparseTensorFormat.Batch
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_COMPRESSED ->
          SparseTensorFormat.Compressed
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_SINGLETON ->
          SparseTensorFormat.Singleton
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_LOOSE_COMPRESSED ->
          SparseTensorFormat.LooseCompressed
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_N_OUT_OF_M ->
          SparseTensorFormat.NOutOfM

      method dimensions_to_levels =
        mlir_sparse_tensor_encoding_attr_get_dim_to_lvl self#raw |> AffineMap.from_raw

      method levels_to_dimensions =
        mlir_sparse_tensor_encoding_attr_get_lvl_to_dim self#raw |> AffineMap.from_raw

      method positions_bit_width = mlir_sparse_tensor_encoding_attr_get_pos_width self#raw

      method coordinates_bit_width =
        mlir_sparse_tensor_encoding_attr_get_crd_width self#raw

      method explicit_value =
        mlir_sparse_tensor_encoding_attr_get_explicit_val self#raw |> Attribute.from_raw

      method implicit_value =
        mlir_sparse_tensor_encoding_attr_get_implicit_val self#raw |> Attribute.from_raw

      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_attribute_get_type self#raw |> Type.from_raw
      method id = mlir_attribute_get_type_id self#raw |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t

  let get
        ctx
        level_types
        dim_to_lvl
        lvl_to_dim_opt
        pos_width_opt
        crd_width_opt
        explicit_val_opt
        implicit_val_opt
    =
    let lvl_to_dim =
      Option.map (fun map -> map#raw) lvl_to_dim_opt
      |> Option.value
           ~default:
             (Ctypes_memory.make
                ?finalise:(Some (fun map -> setf map MlirAffineMap.ptr null))
                MlirAffineMap.t)
    in
    let lvl_type_array = CArray.of_list uint64_t level_types in
    mlir_sparse_tensor_encoding_attr_get
      ctx#raw
      (List.length level_types |> Intptr.of_int)
      (CArray.start lvl_type_array)
      dim_to_lvl#raw
      lvl_to_dim
      (Option.value pos_width_opt ~default:0)
      (Option.value crd_width_opt ~default:0)
      (Option.map (fun attr -> attr#raw) explicit_val_opt
       |> Option.value ~default:(mlir_attribute_get_null ()))
      (Option.map (fun attr -> attr#raw) implicit_val_opt
       |> Option.value ~default:(mlir_attribute_get_null ()))
    |> from_raw
end

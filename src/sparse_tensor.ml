open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Ir.Ir
open Support
open Affine_map.AffineMap
open Utils

module LevelNonDefaultProperty = struct
  type t =
    | NonUnique
    | NonOrdered
    | StructureOfArrays
  [@@deriving ord]
end

module LevelNonDefaultProperties = Set.Make (LevelNonDefaultProperty)

module LevelType = struct
  type raw = Unsigned.uint64

  type t =
    | Dense of raw
    | Batch of raw
    | Compressed of LevelNonDefaultProperties.t * raw
    | LooseCompressed of LevelNonDefaultProperties.t * raw
    | Singleton of LevelNonDefaultProperties.t * raw
    | Structured of int * int * raw

  let to_array =
    LevelNonDefaultProperties.to_list
    >> List.map (fun prop ->
      match prop with
      | LevelNonDefaultProperty.NonUnique ->
        MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_NON_UNIQUE
      | LevelNonDefaultProperty.NonOrdered ->
        MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_NON_ORDERED
      | LevelNonDefaultProperty.StructureOfArrays ->
        MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_SOA)
    >> CArray.of_list MlirSparseTensorLevelPropertyNondefault.t


  let raw = function
    | Dense raw -> raw
    | Batch raw -> raw
    | Compressed (_, raw) -> raw
    | LooseCompressed (_, raw) -> raw
    | Singleton (_, raw) -> raw
    | Structured (_, _, raw) -> raw


  let dense () =
    let prop_array = CArray.of_list MlirSparseTensorLevelPropertyNondefault.t [] in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_DENSE
        (CArray.start prop_array)
        Unsigned.UInt.zero
        Unsigned.UInt.zero
        Unsigned.UInt.zero
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a dense level type!" |> raise
    else Dense raw


  let batch () =
    let prop_array = CArray.of_list MlirSparseTensorLevelPropertyNondefault.t [] in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_BATCH
        (CArray.start prop_array)
        Unsigned.UInt.zero
        Unsigned.UInt.zero
        Unsigned.UInt.zero
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a batch level type!" |> raise
    else Batch raw


  let compressed props =
    let props_set = LevelNonDefaultProperties.of_list props in
    let prop_array = to_array props_set in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_COMPRESSED
        (CArray.start prop_array)
        (LevelNonDefaultProperties.cardinal props_set |> Unsigned.UInt.of_int)
        Unsigned.UInt.zero
        Unsigned.UInt.zero
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a singleton level type!" |> raise
    else Compressed (props_set, raw)


  let loose_compressed props =
    let props_set = LevelNonDefaultProperties.of_list props in
    let prop_array = to_array props_set in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_LOOSE_COMPRESSED
        (CArray.start prop_array)
        (LevelNonDefaultProperties.cardinal props_set |> Unsigned.UInt.of_int)
        Unsigned.UInt.zero
        Unsigned.UInt.zero
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a singleton level type!" |> raise
    else LooseCompressed (props_set, raw)


  let singleton props =
    let props_set = LevelNonDefaultProperties.of_list props in
    let prop_array = to_array props_set in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_SINGLETON
        (CArray.start prop_array)
        (LevelNonDefaultProperties.cardinal props_set |> Unsigned.UInt.of_int)
        Unsigned.UInt.zero
        Unsigned.UInt.zero
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a singleton level type!" |> raise
    else Singleton (props_set, raw)


  let structured n m =
    let prop_array = CArray.of_list MlirSparseTensorLevelPropertyNondefault.t [] in
    let raw =
      mlir_sparse_tensor_encoding_attr_build_lvl_type
        MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_N_OUT_OF_M
        (CArray.start prop_array)
        Unsigned.UInt.zero
        (Unsigned.UInt.of_int n)
        (Unsigned.UInt.of_int m)
    in
    if Unsigned.UInt64.compare raw Unsigned.UInt64.zero == 0
    then Error "Unable to construct a singleton level type!" |> raise
    else Structured (n, m, raw)
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
        let raw =
          mlir_sparse_tensor_encoding_attr_get_lvl_type self#raw (Intptr.of_int lvl)
        in
        match
          mlir_sparse_tensor_encoding_attr_get_lvl_fmt self#raw (Intptr.of_int lvl)
        with
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_DENSE ->
          LevelType.Dense raw
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_BATCH ->
          LevelType.Batch raw
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_COMPRESSED ->
          let props = ref LevelNonDefaultProperties.empty in
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_ORDERED
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonOrdered !props
          else ();
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_UNIQUE
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonUnique !props
          else ();
          LevelType.Compressed (!props, raw)
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_LOOSE_COMPRESSED ->
          let props = ref LevelNonDefaultProperties.empty in
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_ORDERED
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonOrdered !props
          else ();
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_UNIQUE
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonUnique !props
          else ();
          LevelType.LooseCompressed (!props, raw)
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_SINGLETON ->
          let props = ref LevelNonDefaultProperties.empty in
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_ORDERED
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonOrdered !props
          else ();
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault
                    .MLIR_SPARSE_PROPERTY_NON_UNIQUE
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add LevelNonDefaultProperty.NonUnique !props
          else ();
          if
            Int64.compare
              (Int64.logand
                 (Unsigned.UInt64.to_int64 raw)
                 (List.assoc
                    MlirSparseTensorLevelPropertyNondefault.MLIR_SPARSE_PROPERTY_SOA
                    MlirSparseTensorLevelPropertyNondefault.mapping))
              Int64.zero
            > 0
          then
            props
            := LevelNonDefaultProperties.add
                 LevelNonDefaultProperty.StructureOfArrays
                 !props
          else ();
          LevelType.Singleton (!props, raw)
        | MlirSparseTensorLevelFormat.MLIR_SPARSE_TENSOR_LEVEL_N_OUT_OF_M ->
          let n =
            mlir_sparse_tensor_encoding_attr_get_structured_n raw |> Unsigned.UInt.to_int
          and m =
            mlir_sparse_tensor_encoding_attr_get_structured_m raw |> Unsigned.UInt.to_int
          in
          LevelType.Structured (n, m, raw)

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
    let lvl_type_array = CArray.of_list uint64_t (List.map LevelType.raw level_types) in
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

module SparseTensor = struct

  let yield results location =
    OpBuilder.get "sparse_tensor.yield" location
    |> OpBuilder.add_operands results
    |> OpBuilder.build false

  let reduce left right identity location ~init =
    OpBuilder.get "sparse_tensor.reduce" location
    |> OpBuilder.add_operands [ left ]
    |> OpBuilder.add_operands [ right ]
    |> OpBuilder.add_operands [ identity ]
    |> OpBuilder.add_regions
      [ let region = Region.get () in
        let inputs = [ left#get_type, location; right#get_type, location ]
        in
        let body = Block.get inputs in
        let results = init body in
        body#append_operation @@ yield results location;
        region#append_block body;
        region
      ]
    |> OpBuilder.build true

end

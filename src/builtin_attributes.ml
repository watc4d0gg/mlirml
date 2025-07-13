open Ctypes
open Bindings.Types
open Bindings.Functions
open Affine_map.AffineMap
open Error
open Ir.Ir
open Support
open Utils

module AffineMapAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_aaffine_map raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to an AffineMapAttr")
          |> raise

      method value = mlir_affine_map_attr_get_value self#raw |> AffineMap.from_raw
      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_attribute_get_type self#raw |> Type.from_raw
      method id = mlir_affine_map_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t
  let get map = mlir_affine_map_attr_get map#raw |> from_raw
end

module ArrayAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_aarray raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to an ArrayAttr")
          |> raise

      method elements = mlir_array_attr_get_num_elements self#raw |> Intptr.to_int

      method element pos =
        mlir_array_attr_get_element self#raw (Intptr.of_int pos) |> Attribute.from_raw

      method iter_elements : f:(Attribute.t -> unit) -> unit = fun ~f ->
        List.init self#elements Fun.id |> List.iter (fun index -> self#element index |> f)

      method iteri_elements : f:(int -> Attribute.t -> unit) -> unit = fun ~f ->
        List.init self#elements Fun.id
        |> List.iter (fun index -> self#element index |> f index)

      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_attribute_get_type self#raw |> Type.from_raw
      method id = mlir_string_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t

  let get ctx elems =
    let attribute_array =
      CArray.of_list MlirAttribute.t (List.map (fun attr -> attr#raw) elems)
    in
    mlir_array_attr_get
      ctx#raw
      (List.length elems |> Intptr.of_int)
      (CArray.start attribute_array)
    |> from_raw
end

module TypedAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_atype raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a TypedAttr")
          |> raise

      method value = mlir_type_attr_get_value self#raw |> Type.from_raw
      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_type_attr_get_value self#raw |> Type.from_raw
      method id = mlir_type_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t
  let get t = mlir_type_attr_get t#raw |> from_raw
end

module FloatAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_afloat raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a FloatAttr")
          |> raise

      method value = mlir_float_attr_get_value_double self#raw
      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_type_attr_get_value self#raw |> Type.from_raw
      method id = mlir_float_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t

  let get t value location =
    mlir_float_attr_double_get_checked location#raw t#raw value
    |> not_null mlir_attribute_is_null "Unable to create a float attribute"
    |> from_raw
end

module IntegerAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_ainteger raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to an IntegerAttr")
          |> raise

      method value = mlir_integer_attr_get_value_int self#raw
      method signless_value = mlir_integer_attr_get_value_sint self#raw
      method unsigned_value = mlir_integer_attr_get_value_uint self#raw
      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_type_attr_get_value self#raw |> Type.from_raw
      method id = mlir_integer_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t

  let get t value =
    mlir_integer_attr_get t#raw (Int64.of_int value)
    |> not_null mlir_attribute_is_null "Unable to create an integer attribute"
    |> from_raw
end

module StringAttr = struct
  class t raw =
    object (self)
      initializer
        if mlir_attribute_is_astring raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a StringAttr")
          |> raise

      method value = mlir_string_attr_get_value self#raw |> string_ref_as_string
      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_type_attr_get_value self#raw |> Type.from_raw
      method id = mlir_string_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t

  let get ctx value =
    mlir_string_attr_get ctx#raw (mlir_string_ref_create_from_cstring value)
    |> not_null mlir_attribute_is_null "Unable to create a string attribute"
    |> from_raw


  let get_typed t value =
    mlir_string_attr_typed_get t#raw (mlir_string_ref_create_from_cstring value)
    |> not_null mlir_attribute_is_null "Unable to create a string attribute"
    |> from_raw
end

module DenseArrayAttr = struct
  class ['a] t element_getter init_check type_name raw =
    object (self)
      initializer
        if init_check raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a "
             ^ type_name)
          |> raise

      method elements = mlir_dense_array_get_num_elements self#raw |> Intptr.to_int

      method element : int -> 'a =
        fun index -> element_getter self#raw (Intptr.of_int index)

      method iter_elements : f:('a -> unit) -> unit = fun ~f ->
        List.init self#elements Fun.id |> List.iter (fun index -> self#element index |> f)

      method iteri_elements : f:(int -> 'a -> unit) -> unit = fun ~f ->
        List.init self#elements Fun.id
        |> List.iter (fun index -> self#element index |> f index)

      method context = mlir_attribute_get_context self#raw |> Context.from_raw
      method t = mlir_attribute_get_type self#raw |> Type.from_raw
      method id = mlir_dense_array_attr_get_type_id () |> TypeId.from_raw
      method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end
end

module DenseBoolArrayAttr = struct
  class t =
    [bool] DenseArrayAttr.t
      mlir_dense_bool_array_get_element
      mlir_attribute_is_adense_bool_array
      "DenseBoolArrayAttr"

  let from_raw = new t

  let get ctx elements =
    let elt_array = CArray.of_list int (List.map Bool.to_int elements) in
    mlir_dense_bool_array_get
      ctx#raw
      (List.length elements |> Intptr.of_int)
      (CArray.start elt_array)
    |> from_raw
end

module DenseInt8ArrayAttr = struct
  class t =
    [int] DenseArrayAttr.t
      mlir_dense_i8_array_get_element
      mlir_attribute_is_adense_i8_array
      "DenseInt8ArrayAttr"

  let from_raw = new t

  let get ctx elements =
    let elt_array = CArray.of_list int8_t elements in
    mlir_dense_i8_array_get
      ctx#raw
      (List.length elements |> Intptr.of_int)
      (CArray.start elt_array)
    |> from_raw
end

module DenseInt16ArrayAttr = struct
  class t =
    [int] DenseArrayAttr.t
      mlir_dense_i16_array_get_element
      mlir_attribute_is_adense_i16_array
      "DenseInt16ArrayAttr"

  let from_raw = new t

  let get ctx elements =
    let elt_array = CArray.of_list int16_t elements in
    mlir_dense_i16_array_get
      ctx#raw
      (List.length elements |> Intptr.of_int)
      (CArray.start elt_array)
    |> from_raw
end

module DenseInt32ArrayAttr = struct
  class t =
    [int32] DenseArrayAttr.t
      mlir_dense_i32_array_get_element
      mlir_attribute_is_adense_i32_array
      "DenseInt32ArrayAttr"

  let from_raw = new t

  let get ctx elements =
    let elt_array = CArray.of_list int32_t elements in
    mlir_dense_i32_array_get
      ctx#raw
      (List.length elements |> Intptr.of_int)
      (CArray.start elt_array)
    |> from_raw
end

module DenseInt64ArrayAttr = struct
  class t =
    [int64] DenseArrayAttr.t
      mlir_dense_i64_array_get_element
      mlir_attribute_is_adense_i64_array
      "DenseInt64ArrayAttr"

  let from_raw = new t

  let get ctx elements =
    let elt_array = CArray.of_list int64_t elements in
    mlir_dense_i64_array_get
      ctx#raw
      (List.length elements |> Intptr.of_int)
      (CArray.start elt_array)
    |> from_raw
end

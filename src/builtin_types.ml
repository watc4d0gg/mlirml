open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Ir.Ir
open Support
open Utils

module FunctionType = struct
  class t raw =
    object (self)
      initializer
        if mlir_type_is_afunction raw
        then ()
        else
          Error
            ("Unable to cast the type "
             ^ print_raw_as_string mlir_type_print raw
             ^ " to a Function")
          |> raise

      method inputs = mlir_function_type_get_num_inputs self#raw |> Intptr.to_int

      method input index =
        mlir_function_type_get_input self#raw (Intptr.of_int index) |> Type.from_raw

      method iter_inputs ~f =
        List.init self#inputs Fun.id |> List.iter (fun index -> self#input index |> f)

      method iteri_inputs ~f =
        List.init self#inputs Fun.id
        |> List.iter (fun index -> self#input index |> f index)

      method map_inputs : 'a. f:(Type.t -> 'a) -> 'a list =
        fun ~f -> List.init self#inputs self#input |> List.map f

      method results = mlir_function_type_get_num_results self#raw |> Intptr.to_int

      method result index =
        mlir_function_type_get_result self#raw (Intptr.of_int index) |> Type.from_raw

      method iter_results ~f =
        List.init self#results Fun.id |> List.iter (fun index -> self#result index |> f)

      method iteri_results ~f =
        List.init self#results Fun.id
        |> List.iter (fun index -> self#result index |> f index)

      method map_results : 'a. f:(Type.t -> 'a) -> 'a list =
        fun ~f -> List.init self#inputs self#input |> List.map f

      method context = mlir_type_get_context self#raw |> Context.from_raw
      method id = mlir_type_get_type_id self#raw |> TypeId.from_raw
      method dialect = mlir_type_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let from_raw = new t
  let cast t = from_raw t#raw

  let get ctx inputs results =
    let input_array = CArray.of_list MlirType.t (List.map (fun t -> t#raw) inputs)
    and result_array = CArray.of_list MlirType.t (List.map (fun t -> t#raw) results) in
    let result =
      mlir_function_type_get
        ctx#raw
        (List.length inputs |> Intptr.of_int)
        (CArray.start input_array)
        (List.length results |> Intptr.of_int)
        (CArray.start result_array)
    in
    if mlir_type_is_null result
    then Error "Invalid function type declaration" |> raise
    else from_raw result
end

module ShapedType = struct
  type dim_size =
    | Static of int
    | Dynamic

  class t raw =
    object (self)
      initializer
        if mlir_type_is_ashaped raw
        then ()
        else
          Error
            ("Unable to cast the type "
             ^ print_raw_as_string mlir_type_print raw
             ^ " to a Shaped type")
          |> raise

      method element_type = mlir_shaped_type_get_element_type self#raw |> Type.from_raw
      method has_rank = mlir_shaped_type_has_rank self#raw

      method rank =
        if self#has_rank
        then Some (mlir_shaped_type_get_rank self#raw |> Int64.to_int)
        else None

      method has_static_shape = mlir_shaped_type_has_static_shape self#raw

      method is_dynamic_dimension dim =
        mlir_shaped_type_is_dynamic_dim self#raw (Intptr.of_int dim)

      method dimension_size dim =
        match self#rank with
        | None -> None
        | Some rank ->
          if dim >= rank
          then None
          else (
            let result = mlir_shaped_type_get_dim_size self#raw (Intptr.of_int dim) in
            Some
              (if mlir_shaped_type_is_dynamic_size result
               then Dynamic
               else Static (Int64.to_int result)))

      method context = mlir_type_get_context self#raw |> Context.from_raw
      method id = mlir_type_get_type_id self#raw |> TypeId.from_raw
      method dialect = mlir_type_get_dialect self#raw |> Dialect.from_raw
      method raw = raw
    end

  let cast t = new t t#raw
end

module TensorType = struct
  class t raw =
    object
      initializer
        if mlir_type_is_atensor raw
        then ()
        else
          Error
            ("Unable to cast the type "
             ^ print_raw_as_string mlir_type_print raw
             ^ " to a Tensor type")
          |> raise

      inherit ShapedType.t raw
    end

  let is_unranked tn = mlir_type_is_aunranked_tensor tn#raw
  let is_ranked tn = mlir_type_is_aranked_tensor tn#raw
end

module RankedTensorType = struct
  class t raw =
    object (self)
      initializer
        if mlir_type_is_aranked_tensor raw
        then ()
        else
          Error
            ("Unable to cast the type "
             ^ print_raw_as_string mlir_type_print raw
             ^ " to a RankedTensor type")
          |> raise

      inherit TensorType.t raw
      method! id = mlir_ranked_tensor_type_get_type_id () |> TypeId.from_raw

      method encoding =
        mlir_ranked_tensor_type_get_encoding self#raw
        |> null_to_opt mlir_attribute_is_null Attribute.from_raw
    end

  let from_raw = new t

  let get dims element encoding location =
    let dim_array =
      List.map
        (fun dim ->
           match dim with
           | ShapedType.Static size -> Int64.of_int size
           | ShapedType.Dynamic -> mlir_shaped_type_get_dynamic_size ())
        dims
      |> CArray.of_list int64_t
    in
    let enc =
      Option.value
        (Option.map (fun attr -> attr#raw) encoding)
        ~default:(mlir_attribute_get_null ())
    in
    mlir_ranked_tensor_type_get_checked
      location#raw
      (List.length dims |> Intptr.of_int)
      (CArray.start dim_array)
      element#raw
      enc
    |> not_null mlir_type_is_null "Not a valid ranked tensor type"
    |> from_raw
end

module UnrankedTensorType = struct
  class t raw =
    object
      initializer
        if mlir_type_is_aunranked_tensor raw
        then ()
        else
          Error
            ("Unable to cast the type "
             ^ print_raw_as_string mlir_type_print raw
             ^ " to a UnrankedTensor type")
          |> raise

      inherit TensorType.t raw
      method! id = mlir_unranked_tensor_type_get_type_id () |> TypeId.from_raw
    end

  let from_raw = new t

  let get element location =
    mlir_unranked_tensor_type_get_checked location#raw element#raw
    |> not_null mlir_type_is_null "Not a valid unranked tensor type"
    |> from_raw
end

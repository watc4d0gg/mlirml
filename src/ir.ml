open Ctypes
open Bindings.Types
open Bindings.Functions
open Error
open Support
open Utils

(* TODO: add not_null asserts to ALL complex factory functions/parsers *)

module Ir = struct
  module DialectHandle = struct
    class t raw =
      object (self)
        method namespace =
          mlir_dialect_handle_get_namespace self#raw |> string_ref_as_string

        method raw = raw
      end

    let from_raw = new t
    let arith = mlir_get_dialect_handle__arith_ >> from_raw
    let func = mlir_get_dialect_handle__func_ >> from_raw
    let linalg = mlir_get_dialect_handle__linalg_ >> from_raw
    let memref = mlir_get_dialect_handle__memref_ >> from_raw
    let scf = mlir_get_dialect_handle__scf_ >> from_raw
    let sparse_tensor = mlir_get_dialect_handle__sparse_tensor_ >> from_raw
    let tensor = mlir_get_dialect_handle__tensor_ >> from_raw
    let transform = mlir_get_dialect_handle__transform_ >> from_raw
  end

  module DialectRegistry = struct
    class t raw =
      object (self)
        method insert_dialect (handle : DialectHandle.t) =
          mlir_dialect_handle_insert_dialect handle#raw self#raw

        method destroy = mlir_dialect_registry_destroy self#raw
        method raw = raw
      end

    let from_raw = new t
    let get = mlir_dialect_registry_create >> from_raw
  end

  module Context = struct
    class t raw =
      object (self)
        method allows_unregistered_dialects =
          mlir_context_get_allow_unregistered_dialects self#raw

        method allow_unregistered_dialects =
          mlir_context_set_allow_unregistered_dialects self#raw

        method registered_dialects =
          mlir_context_get_num_registered_dialects self#raw |> Intptr.to_int

        method register_dialect (handle : DialectHandle.t) =
          mlir_dialect_handle_register_dialect handle#raw self#raw

        method append_dialect_registry (registry : DialectRegistry.t) =
          mlir_context_append_dialect_registry self#raw registry#raw

        method loaded_dialects =
          mlir_context_get_num_loaded_dialects self#raw |> Intptr.to_int

        method enable_multithreading = mlir_context_enable_multithreading self#raw

        method load_all_available_dialects =
          mlir_context_load_all_available_dialects self#raw

        method is_registered_operation name =
          mlir_string_ref_create_from_cstring name
          |> mlir_context_is_registered_operation self#raw

        method set_thread_pool (pool : LLVMThreadPool.t) =
          mlir_context_set_thread_pool self#raw pool#raw

        method destroy = mlir_context_destroy self#raw
        method raw = raw
      end

    let from_raw = new t

    let get registry_opt threading =
      match registry_opt with
      | Some registry ->
        mlir_context_create_with_registry registry#raw threading |> from_raw
      | None -> mlir_context_create_with_threading threading |> from_raw


    let equal c1 c2 = mlir_context_equal c1#raw c2#raw
  end

  module Dialect = struct
    class t raw =
      object (self)
        method context = mlir_dialect_get_context self#raw |> Context.from_raw
        method namespace = mlir_dialect_get_namespace self#raw |> string_ref_as_string
        method raw = raw
      end

    let from_raw = new t
    let load ctx handle = mlir_dialect_handle_load_dialect handle#raw ctx#raw |> from_raw

    let get_or_load ctx name =
      mlir_string_ref_create_from_cstring name
      |> mlir_context_get_or_load_dialect ctx#raw
      |> from_raw


    let equal d1 d2 = mlir_dialect_equal d1#raw d2#raw
  end

  module Type = struct
    class t raw =
      object (self)
        method context = mlir_type_get_context self#raw |> Context.from_raw
        method id = mlir_type_get_type_id self#raw |> TypeId.from_raw
        method dialect = mlir_type_get_dialect self#raw |> Dialect.from_raw
        method raw = raw
      end

    let from_raw = new t

    let parse ctx data =
      mlir_string_ref_create_from_cstring data
      |> mlir_type_parse_get ctx#raw
      |> not_null mlir_type_is_null ("Unable to parse \"" ^ data ^ "\" as a type")
      |> from_raw


    let bfloat16 ctx = mlir_bf16_type_get ctx#raw |> from_raw
    let float16 ctx = mlir_f16_type_get ctx#raw |> from_raw
    let float32 ctx = mlir_f32_type_get ctx#raw |> from_raw
    let float64 ctx = mlir_f64_type_get ctx#raw |> from_raw
    let index ctx = mlir_index_type_get ctx#raw |> from_raw
    let none ctx = mlir_none_type_get ctx#raw |> from_raw
    let is_function t = mlir_type_is_afunction t#raw
    let is_shaped t = mlir_type_is_ashaped t#raw
    let equal t1 t2 = mlir_type_equal t1#raw t2#raw
    let print ~callback t = print_raw mlir_type_print ~callback t#raw
    let dump t = mlir_type_dump t#raw
  end

  module Identifier = struct
    class t raw =
      object (self)
        method context = mlir_identifier_get_context self#raw |> Context.from_raw
        method str = mlir_identifier_str self#raw |> string_ref_as_string
        method raw = raw
      end

    let from_raw = new t

    let get ctx =
      mlir_string_ref_create_from_cstring >> mlir_identifier_get ctx#raw >> from_raw


    let equal i1 i2 = mlir_identifier_equal i1#raw i2#raw
  end

  module Attribute = struct
    class t raw =
      object (self)
        method context = mlir_attribute_get_context self#raw |> Context.from_raw
        method t = mlir_attribute_get_type self#raw |> Type.from_raw
        method id = mlir_attribute_get_type_id self#raw |> TypeId.from_raw
        method dialect = mlir_attribute_get_dialect self#raw |> Dialect.from_raw
        method raw = raw
      end

    let from_raw = new t

    let parse ctx data =
      mlir_string_ref_create_from_cstring data
      |> mlir_attribute_parse_get ctx#raw
      |> not_null
           mlir_attribute_is_null
           ("Unable to parse \"" ^ data ^ "\" as an attribute")
      |> from_raw


    let is_affine_map attr = mlir_attribute_is_aaffine_map attr#raw
    let is_array attr = mlir_attribute_is_aarray attr#raw
    let is_typed attr = mlir_attribute_is_atype attr#raw
    let is_float attr = mlir_attribute_is_afloat attr#raw
    let is_string attr = mlir_attribute_is_astring attr#raw
    let is_dense_bool_array attr = mlir_attribute_is_adense_bool_array attr#raw
    let is_dense_int8_array attr = mlir_attribute_is_adense_i8_array attr#raw
    let is_dense_int16_array attr = mlir_attribute_is_adense_i16_array attr#raw
    let is_dense_int32_array attr = mlir_attribute_is_adense_i32_array attr#raw
    let is_dense_int64_array attr = mlir_attribute_is_adense_i64_array attr#raw

    let is_sparse_tensor_encoding attr =
      mlir_attribute_is_asparse_tensor_encoding_attr attr#raw


    let unit ctx = mlir_unit_attr_get ctx#raw |> from_raw
    let equal a1 a2 = mlir_attribute_equal a1#raw a2#raw
    let print ~callback attr = print_raw mlir_attribute_print ~callback attr#raw
    let dump attr = mlir_attribute_dump attr#raw
  end

  module Location = struct
    class t raw =
      object (self)
        method attribute = mlir_location_get_attribute self#raw |> Attribute.from_raw
        method context = mlir_location_get_context self#raw |> Context.from_raw
        method raw = raw
      end

    let from_raw = new t
    let attribute attr = mlir_location_from_attribute attr#raw |> from_raw

    let file_line_col ctx filename line col =
      mlir_location_file_line_col_get
        ctx#raw
        (mlir_string_ref_create_from_cstring filename)
        (Unsigned.UInt.of_int line)
        (Unsigned.UInt.of_int col)
      |> from_raw


    let call_site callee caller =
      mlir_location_call_site_get callee#raw caller#raw |> from_raw


    let fused ctx locations metadata =
      let loc_array =
        CArray.of_list MlirLocation.t (List.map (fun loc -> loc#raw) locations)
      in
      mlir_location_fused_get
        ctx#raw
        (CArray.length loc_array |> Intptr.of_int)
        (CArray.start loc_array)
        metadata#raw
      |> from_raw


    let name ctx name child =
      let child_loc =
        Option.value
          (Option.map (fun loc -> loc#raw) child)
          ~default:
            (let null_loc = make MlirLocation.t in
             setf null_loc MlirLocation.ptr null;
             null_loc)
      in
      mlir_location_name_get ctx#raw (mlir_string_ref_create_from_cstring name) child_loc
      |> from_raw


    let unknown ctx = mlir_location_unknown_get ctx#raw |> from_raw
    let equal l1 l2 = mlir_location_equal l1#raw l2#raw
    let print ~callback loc = print_raw mlir_location_print ~callback loc#raw
  end

  module OpPrintingFlags = struct
    class t raw =
      object (self)
        method destroy = mlir_op_printing_flags_destroy self#raw
        method raw = raw
      end

    let from_raw = new t
    let get = mlir_op_printing_flags_create >> from_raw

    let elide_large_elements_attributes limit (flags : t) =
      mlir_op_printing_flags_elide_large_elements_attrs flags#raw (Intptr.of_int limit);
      flags


    let elide_large_resource_string limit (flags : t) =
      mlir_op_printing_flags_elide_large_resource_string flags#raw (Intptr.of_int limit);
      flags


    let enable_debug_info enable pretty (flags : t) =
      mlir_op_printing_flags_enable_debug_info flags#raw enable pretty;
      flags


    let print_generic_op_form (flags : t) =
      mlir_op_printing_flags_print_generic_op_form flags#raw;
      flags


    let use_local_scope (flags : t) =
      mlir_op_printing_flags_use_local_scope flags#raw;
      flags


    let skip_regions (flags : t) =
      mlir_op_printing_flags_skip_regions flags#raw;
      flags
  end

  module BytecodeWriterConfig = struct
    class t raw =
      object (self)
        method desired_emit_version version =
          mlir_bytecode_writer_config_desired_emit_version self#raw (Int64.of_int version)

        method destroy = mlir_bytecode_writer_config_destroy self#raw
        method raw = raw
      end

    let from_raw = new t
    let get = mlir_bytecode_writer_config_create >> from_raw
  end

  module AsmState = struct
    class t raw =
      object (self)
        method destroy = mlir_asm_state_destroy self#raw
        method raw = raw
      end

    let from_raw = new t

    let create_for_operation op flags =
      mlir_asm_state_create_for_operation op#raw flags#raw |> from_raw


    let create_for_value value flags =
      mlir_asm_state_create_for_value value#raw flags#raw |> from_raw
  end

  class operation raw =
    object (self)
      method context = mlir_operation_get_context self#raw |> Context.from_raw
      method location = mlir_operation_get_location self#raw |> Location.from_raw

      method id =
        mlir_operation_get_type_id self#raw
        |> null_to_opt mlir_type_idis_null TypeId.from_raw

      method name = mlir_operation_get_name self#raw |> Identifier.from_raw

      method block =
        mlir_operation_get_block self#raw |> null_to_opt mlir_block_is_null (new block)

      method next =
        mlir_operation_get_next_in_block self#raw
        |> null_to_opt mlir_operation_is_null (new operation)

      method parent =
        mlir_operation_get_parent_operation self#raw
        |> null_to_opt mlir_operation_is_null (new operation)

      method first_region =
        mlir_operation_get_first_region self#raw
        |> null_to_opt mlir_region_is_null (new region)

      method regions = mlir_operation_get_num_regions self#raw |> Intptr.to_int

      method region index =
        mlir_operation_get_region self#raw (Intptr.of_int index) |> new region

      method iter_regions : f:(region -> unit) -> unit = fun ~f ->
        List.init self#regions Fun.id |> List.iter (fun index -> self#region index |> f)

      method iteri_regions : f:(int -> region -> unit) -> unit = fun ~f ->
        List.init self#regions Fun.id
        |> List.iter (fun index -> self#region index |> f index)

      method operands = mlir_operation_get_num_operands self#raw |> Intptr.to_int

      method operand index =
        mlir_operation_get_operand self#raw (Intptr.of_int index) |> new value

      method set_operand
        :  'a.
           int
        -> (< raw : MlirValue.t structure
            ; get_type : Type.t
            ; set_type : 'a. (#Type.t as 'a) -> unit
            ; first_use : op_operand option
            ; .. >
            as
            'a)
        -> unit =
        fun index value ->
          mlir_operation_set_operand self#raw (Intptr.of_int index) value#raw

      method iter_operands : f:(value -> unit) -> unit = fun ~f ->
        List.init self#operands Fun.id |> List.iter (fun index -> self#operand index |> f)

      method iteri_operands : f:(int -> value -> unit) -> unit = fun ~f ->
        List.init self#operands Fun.id
        |> List.iter (fun index -> self#operand index |> f index)

      method replace_operands
        :  'a.
           (< raw : MlirValue.t structure
            ; get_type : Type.t
            ; set_type : 'a. (#Type.t as 'a) -> unit
            ; first_use : op_operand option
            ; .. >
            as
            'a)
             list
        -> unit =
        fun new_operands ->
          let operand_array =
            CArray.of_list MlirValue.t (List.map (fun op -> op#raw) new_operands)
          in
          mlir_operation_set_operands
            self#raw
            (CArray.length operand_array |> Intptr.of_int)
            (CArray.start operand_array)

      method results = mlir_operation_get_num_results self#raw |> Intptr.to_int

      method result index =
        mlir_operation_get_result self#raw (Intptr.of_int index) |> new op_result

      method iter_results : f:(op_result -> unit) -> unit = fun ~f ->
        List.init self#results Fun.id |> List.iter (fun index -> self#result index |> f)

      method iteri_results : f:(int -> op_result -> unit) -> unit = fun ~f ->
        List.init self#results Fun.id
        |> List.iter (fun index -> self#result index |> f index)

      method successors = mlir_operation_get_num_successors self#raw |> Intptr.to_int

      method successor index =
        mlir_operation_get_successor self#raw (Intptr.of_int index) |> new block

      method set_successor index (block : block) =
        mlir_operation_set_successor self#raw (Intptr.of_int index) block#raw

      method iter_successors : f:(block -> unit) -> unit = fun ~f ->
        List.init self#successors Fun.id
        |> List.iter (fun index -> self#successor index |> f)

      method iteri_successors : f:(int -> block -> unit) -> unit = fun ~f ->
        List.init self#successors Fun.id
        |> List.iter (fun index -> self#successor index |> f index)

      method has_inherent_attribute name =
        mlir_operation_has_inherent_attribute_by_name
          self#raw
          (mlir_string_ref_create_from_cstring name)

      method inherent_attribute name =
        if self#has_inherent_attribute name
        then
          Some
            (mlir_operation_get_inherent_attribute_by_name
               self#raw
               (mlir_string_ref_create_from_cstring name)
             |> Attribute.from_raw)
        else None

      method set_inherent_attribute : 'a. string -> (#Attribute.t as 'a) -> unit =
        fun name attr ->
          mlir_operation_set_inherent_attribute_by_name
            self#raw
            (mlir_string_ref_create_from_cstring name)
            attr#raw

      method discardable_attributes =
        mlir_operation_get_num_discardable_attributes self#raw |> Intptr.to_int

      method discardable_attribute name =
        mlir_operation_get_discardable_attribute_by_name
          self#raw
          (mlir_string_ref_create_from_cstring name)
        |> null_to_opt mlir_attribute_is_null Attribute.from_raw

      method put_discardable_attribute : 'a. string -> (#Attribute.t as 'a) -> unit =
        fun name attr ->
          mlir_operation_set_discardable_attribute_by_name
            self#raw
            (mlir_string_ref_create_from_cstring name)
            attr#raw

      method discardable_attribute_at index =
        let entry =
          mlir_operation_get_discardable_attribute self#raw (Intptr.of_int index)
        in
        ( getf entry MlirNamedAttribute.name |> Identifier.from_raw
        , getf entry MlirNamedAttribute.attribute |> Attribute.from_raw )

      method remove_discardable_attribute name =
        mlir_operation_remove_discardable_attribute_by_name
          self#raw
          (mlir_string_ref_create_from_cstring name)

      method iter_discardable_attributes : f:(Identifier.t * Attribute.t -> unit) -> unit = fun ~f ->
        List.init self#discardable_attributes Fun.id
        |> List.iter (fun index ->
          let entry =
            mlir_operation_get_discardable_attribute self#raw (Intptr.of_int index)
          in
          f
            ( getf entry MlirNamedAttribute.name |> Identifier.from_raw
            , getf entry MlirNamedAttribute.attribute |> Attribute.from_raw ))

      method iteri_discardable_attributes : f:(int -> Identifier.t * Attribute.t -> unit) -> unit = fun ~f ->
        List.init self#discardable_attributes Fun.id
        |> List.iter (fun index ->
          let entry =
            mlir_operation_get_discardable_attribute self#raw (Intptr.of_int index)
          in
          f
            index
            ( getf entry MlirNamedAttribute.name |> Identifier.from_raw
            , getf entry MlirNamedAttribute.attribute |> Attribute.from_raw ))

      method verify = mlir_operation_verify self#raw
      method move_after (op : operation) = mlir_operation_move_after self#raw op#raw
      method move_before (op : operation) = mlir_operation_move_before self#raw op#raw
      method clone = mlir_operation_clone self#raw |> new operation
      method remove_from_parent = mlir_operation_remove_from_parent self#raw
      method destroy = mlir_operation_destroy self#raw
      method raw = raw
    end

  and region raw =
    object (self)
      method first =
        mlir_region_get_first_block self#raw |> null_to_opt mlir_block_is_null (new block)

      method append_block (block : block) =
        mlir_region_append_owned_block self#raw block#raw

      method insert_block index (block : block) =
        mlir_region_insert_owned_block self#raw (Intptr.of_int index) block#raw

      method insert_block_after (ref_opt : block option) (block : block) =
        let ref =
          Option.value
            (Option.map (fun op -> op#raw) ref_opt)
            ~default:
              (let new_ref = make MlirBlock.t in
               setf new_ref MlirBlock.ptr null;
               new_ref)
        in
        mlir_region_insert_owned_block_after self#raw ref block#raw

      method insert_block_before (ref_opt : block option) (block : block) =
        let ref =
          Option.value
            (Option.map (fun op -> op#raw) ref_opt)
            ~default:
              (let new_ref = make MlirBlock.t in
               setf new_ref MlirBlock.ptr null;
               new_ref)
        in
        mlir_region_insert_owned_block_before self#raw ref block#raw

      method iter_blocks : f:(block -> unit) -> unit = fun ~f ->
        let rec iter current =
          match current with
          | Some block ->
            f block;
            iter block#next
          | None -> ()
        in
        iter self#first

      method iteri_blocks : f:(int -> block -> unit) -> unit = fun ~f ->
        let rec iteri current index =
          match current with
          | Some block ->
            f index block;
            iteri block#next (index + 1)
          | None -> ()
        in
        iteri self#first 0

      method next =
        mlir_region_get_next_in_operation self#raw
        |> null_to_opt mlir_region_is_null (new region)

      method take_body (region : region) = mlir_region_take_body self#raw region#raw
      method destroy = mlir_region_destroy self#raw
      method raw = raw
    end

  and block raw =
    object (self)
      method parent_operation =
        mlir_block_get_parent_operation self#raw
        |> null_to_opt mlir_operation_is_null (new operation)

      method parent_region = mlir_block_get_parent_region self#raw |> new region

      method next =
        mlir_block_get_next_in_region self#raw
        |> null_to_opt mlir_block_is_null (new block)

      method first =
        mlir_block_get_first_operation self#raw
        |> null_to_opt mlir_operation_is_null (new operation)

      method terminator =
        mlir_block_get_terminator self#raw
        |> null_to_opt mlir_operation_is_null (new operation)

      method append_operation (op : operation) =
        mlir_block_append_owned_operation self#raw op#raw

      method insert_operation index (op : operation) =
        mlir_block_insert_owned_operation self#raw (Intptr.of_int index) op#raw

      method insert_operation_after (ref_opt : operation option) (op : operation) =
        let ref =
          Option.value
            (Option.map (fun op -> op#raw) ref_opt)
            ~default:
              (let new_ref = make MlirOperation.t in
               setf new_ref MlirOperation.ptr null;
               new_ref)
        in
        mlir_block_insert_owned_operation_after self#raw ref op#raw

      method insert_operation_before (ref_opt : operation option) (op : operation) =
        let ref =
          Option.value
            (Option.map (fun op -> op#raw) ref_opt)
            ~default:
              (let new_ref = make MlirOperation.t in
               setf new_ref MlirOperation.ptr null;
               new_ref)
        in
        mlir_block_insert_owned_operation_before self#raw ref op#raw

      method iter_operations : f:(operation -> unit) -> unit =
        fun ~f ->
          let rec iter current =
            match current with
            | Some op ->
              f op;
              iter op#next
            | None -> ()
          in
          iter self#first

      method iteri_operations : f:(int -> operation -> unit) -> unit =
        fun ~f ->
          let rec iteri current index =
            match current with
            | Some op ->
              f index op;
              iteri op#next (index + 1)
            | None -> ()
          in
          iteri self#first 0

      method arguments = mlir_block_get_num_arguments self#raw |> Intptr.to_int

      method argument index =
        mlir_block_get_argument self#raw (Intptr.of_int index) |> new block_argument

      method add_argument : 'a. (#Type.t as 'a) -> Location.t -> block_argument =
        fun t loc -> mlir_block_add_argument self#raw t#raw loc#raw |> new block_argument

      method insert_argument : 'a. int -> (#Type.t as 'a) -> Location.t -> block_argument
          =
        fun index t loc ->
          mlir_block_insert_argument self#raw (Intptr.of_int index) t#raw loc#raw
          |> new block_argument

      method erase_argument index =
        mlir_block_erase_argument self#raw (Unsigned.UInt.of_int index)

      method iter_arguments : f:(block_argument -> unit) -> unit = fun ~f ->
        List.init self#arguments Fun.id
        |> List.iter (fun index -> self#argument index |> f)

      method iteri_arguments : f:(int -> block_argument -> unit) -> unit = fun ~f ->
        List.init self#arguments Fun.id
        |> List.iter (fun index -> self#argument index |> f index)

      method detach = mlir_block_detach self#raw
      method destroy = mlir_block_destroy self#raw
      method raw = raw
    end

  and value raw =
    object (self)
      method get_type = mlir_value_get_type self#raw |> Type.from_raw

      method set_type : 'a. (#Type.t as 'a) -> unit =
        fun t -> mlir_value_set_type self#raw t#raw

      method first_use =
        mlir_value_get_first_use self#raw
        |> null_to_opt mlir_op_operand_is_null (new op_operand)

      method raw = raw
    end

  and op_result raw =
    object (self)
      inherit value raw
      method owner = mlir_op_result_get_owner self#raw |> new operation
      method number = mlir_op_result_get_result_number self#raw |> Intptr.to_int
    end

  and block_argument raw =
    object (self)
      inherit value raw
      method owner = mlir_block_argument_get_owner self#raw |> new block
      method number = mlir_block_argument_get_arg_number self#raw |> Intptr.to_int

      method! set_type : 'a. (#Type.t as 'a) -> unit =
        fun t -> mlir_block_argument_set_type self#raw t#raw
    end

  and op_operand raw =
    object (self)
      method value = mlir_op_operand_get_value self#raw |> new value
      method owner = mlir_op_operand_get_owner self#raw |> new operation
      method number = mlir_op_operand_get_operand_number self#raw |> Unsigned.UInt.to_int

      method next_use =
        mlir_op_operand_get_next_use self#raw
        |> null_to_opt mlir_op_operand_is_null (new op_operand)

      method raw = raw
    end

  module Value = struct
    class t = value

    let from_raw = new t

    (* val is_block_argument : t -> bool
    val is_op_result : t -> bool *)
    let replace_all_uses_of_with of_val with_val =
      mlir_value_replace_all_uses_of_with of_val#raw with_val#raw


    let equal v1 v2 = mlir_value_equal v1#raw v2#raw
    let print ~callback v = print_raw mlir_value_print ~callback v#raw

    let print_as_operand state ~callback v =
      print_raw (fun raw -> mlir_value_print_as_operand raw state#raw) ~callback v#raw


    let dump v = mlir_value_dump v#raw
  end

  module OpOperand = struct
    class t = op_operand

    let from_raw = new t
  end

  module BlockArgument = struct
    class t = block_argument

    let from_raw = new t
  end

  module OpResult = struct
    class t = op_result

    let from_raw = new t
  end

  module Operation = struct
    class t = operation

    let from_raw = new t

    let parse ctx source name =
      mlir_operation_create_parse
        ctx#raw
        (mlir_string_ref_create_from_cstring source)
        (mlir_string_ref_create_from_cstring name)
      |> from_raw


    let equal o1 o2 = mlir_operation_equal o1#raw o2#raw
    let print ~callback op = print_raw mlir_operation_print ~callback op#raw

    let print_with_flags flags ~callback op =
      print_raw
        (fun raw -> mlir_operation_print_with_flags raw flags#raw)
        ~callback
        op#raw


    let print_with_state state ~callback op =
      print_raw
        (fun raw -> mlir_operation_print_with_state raw state#raw)
        ~callback
        op#raw


    let write_bytecode ~callback op =
      print_raw mlir_operation_write_bytecode ~callback op#raw


    let write_bytecode_with_config config ~callback op =
      print_raw
        (fun raw -> mlir_operation_write_bytecode_with_config raw config#raw)
        ~callback
        op#raw
      |> LogicalResult.from_raw


    let dump op = mlir_operation_dump op#raw

    type walk_result =
      | Advance
      | Interrupt
      | Skip

    type walk_order =
      | PreOrder
      | PostOrder

    let walk ~callback order op =
      let funptr =
        coerce
          (Foreign.funptr (MlirOperation.t @-> ptr void @-> returning MlirWalkResult.t))
          MlirOperationWalkCallback.t
          (fun op _ ->
             match callback (from_raw op) with
             | Advance -> MlirWalkResultAdvance
             | Interrupt -> MlirWalkResultInterrupt
             | Skip -> MlirWalkResultSkip)
      in
      mlir_operation_walk
        op#raw
        funptr
        null
        (match order with
         | PreOrder -> MlirWalkPreOrder
         | PostOrder -> MlirWalkPostOrder)
  end

  module Block = struct
    class t = block

    let from_raw = new t

    let get type_locs =
      let types, locations = List.split type_locs in
      let type_array = CArray.of_list MlirType.t (List.map (fun t -> t#raw) types) in
      let loc_array =
        CArray.of_list MlirLocation.t (List.map (fun loc -> loc#raw) locations)
      in
      mlir_block_create
        (List.length type_locs |> Intptr.of_int)
        (CArray.start type_array)
        (CArray.start loc_array)
      |> from_raw


    let equal b1 b2 = mlir_block_equal b1#raw b2#raw
    let print ~callback block = print_raw mlir_block_print ~callback block#raw
  end

  module Region = struct
    class t = region

    let from_raw = new t
    let get = mlir_region_create >> from_raw
    let equal r1 r2 = mlir_region_equal r1#raw r2#raw
  end

  module Module = struct
    class t raw =
      object (self)
        method context = mlir_module_get_context self#raw |> Context.from_raw
        method body = mlir_module_get_body self#raw |> Block.from_raw
        method to_operation = mlir_module_get_operation self#raw |> Operation.from_raw
        method destroy = mlir_module_destroy self#raw
        method raw = raw
      end

    let from_raw = new t
    let empty loc = mlir_module_create_empty loc#raw |> from_raw

    let parse ctx data =
      mlir_string_ref_create_from_cstring data
      |> mlir_module_create_parse ctx#raw
      |> from_raw


    let from_operation op =
      mlir_module_from_operation op#raw |> null_to_opt mlir_module_is_null from_raw
  end

  module SymbolTable = struct
    class t raw =
      object (self)
        method lookup name =
          mlir_string_ref_create_from_cstring name
          |> mlir_symbol_table_lookup self#raw
          |> null_to_opt mlir_operation_is_null Operation.from_raw

        method insert (op : Operation.t) =
          mlir_symbol_table_insert self#raw op#raw |> Attribute.from_raw

        method erase (op : Operation.t) = mlir_symbol_table_erase self#raw op#raw
        method destroy = mlir_symbol_table_destroy self#raw
        method raw = raw
      end

    let from_raw = new t

    let symbol_attribute_name =
      mlir_symbol_table_get_symbol_attribute_name >> string_ref_as_string


    let visibility_attribute_name =
      mlir_symbol_table_get_visibility_attribute_name >> string_ref_as_string


    let get op =
      mlir_symbol_table_create op#raw |> null_to_opt mlir_symbol_table_is_null from_raw


    let replace_all_symbol_uses old_symbol new_symbol op =
      mlir_symbol_table_replace_all_symbol_uses
        (mlir_string_ref_create_from_cstring old_symbol)
        (mlir_string_ref_create_from_cstring new_symbol)
        op#raw
      |> LogicalResult.from_raw


    let walk all_sym_uses_visible ~callback op =
      let funptr =
        coerce
          (Foreign.funptr (MlirOperation.t @-> bool @-> ptr void @-> returning void))
          (static_funptr (MlirOperation.t @-> bool @-> ptr void @-> returning void))
          (fun op flag _ -> callback (Operation.from_raw op) flag)
      in
      mlir_symbol_table_walk_symbol_tables op#raw all_sym_uses_visible funptr null
  end

  module OpBuilder = struct
    type t = MlirOperationState.t structure ptr

    let get name location =
      allocate
        MlirOperationState.t
        (mlir_operation_state_get (mlir_string_ref_create_from_cstring name) location#raw)


    let destroy = to_voidp >> Root.release

    let add_results types builder =
      let type_array = CArray.of_list MlirType.t (List.map (fun t -> t#raw) types) in
      mlir_operation_state_add_results
        builder
        (List.length types |> Intptr.of_int)
        (CArray.start type_array);
      builder


    let add_operands values builder =
      let value_array =
        CArray.of_list MlirValue.t (List.map (fun value -> value#raw) values)
      in
      mlir_operation_state_add_operands
        builder
        (List.length values |> Intptr.of_int)
        (CArray.start value_array);
      builder


    let add_regions regions builder =
      let region_array =
        CArray.of_list MlirRegion.t (List.map (fun reg -> reg#raw) regions)
      in
      mlir_operation_state_add_owned_regions
        builder
        (List.length regions |> Intptr.of_int)
        (CArray.start region_array);
      builder


    let add_successors blocks builder =
      let block_array =
        CArray.of_list MlirBlock.t (List.map (fun block -> block#raw) blocks)
      in
      mlir_operation_state_add_successors
        builder
        (List.length blocks |> Intptr.of_int)
        (CArray.start block_array);
      builder


    let add_attributes attributes builder =
      let attribute_array =
        CArray.of_list
          MlirNamedAttribute.t
          (List.map
             (fun (id, attr) -> mlir_named_attribute_get id#raw attr#raw)
             attributes)
      in
      mlir_operation_state_add_attributes
        builder
        (List.length attributes |> Intptr.of_int)
        (CArray.start attribute_array);
      builder


    let enable_result_type_inference builder =
      mlir_operation_state_enable_result_type_inference builder;
      builder


    let build verify builder =
      let result = mlir_operation_create builder in
      if mlir_operation_is_null result
      then Error "Unable to build the operation" |> raise
      else if verify && mlir_operation_verify result |> not
      then Error "Unable to verify the operation" |> raise
      else Operation.from_raw result
  end
end

open Ctypes
open Bindings.Types
open Bindings.Functions
open Utils

(* Support *)

exception Error of string

module Support = struct
  module LogicalResult = struct
    type t =
      | Success
      | Failure

    let from_raw raw =
      if mlir_logical_result_is_success raw
      then Success
      else if mlir_logical_result_is_failure raw
      then Failure
      else Error "Unknown logical result!" |> raise
  end

  module LLVMThreadPool = struct
    type t = MlirLlvmThreadPool.t structure

    let create = mlir_llvm_thread_pool_create
    let destroy = mlir_llvm_thread_pool_destroy
  end

  module TypeId = struct
    type t = MlirTypeID.t structure

    let create data ctype =
      if alignment ctype != 8
      then Error "TypeID pointer must be 8-byte aligned" |> raise
      else mlir_type_idcreate (to_voidp (allocate ctype data))


    let equal = mlir_type_idequal
    let hash = mlir_type_idhash_value
  end

  module TypeIdAllocator = struct
    type t = MlirTypeIDAllocator.t structure

    let create = mlir_type_idallocator_create
    let destroy = mlir_type_idallocator_destroy
    let allocated_type_id = mlir_type_idallocator_allocate_type_id
  end
end

(* IR *)

(* TODO: in our api, raise an exceptions on nullity during creation of objects, since creation calls are to succeed all the time with the right input *)

let as_string string_ref = getf string_ref MlirStringRef.data

module Ir = struct
  module Context = struct
    type t = MlirContext.t structure

    let create registry_opt threading =
      match registry_opt with
      | Some registry -> mlir_context_create_with_registry registry threading
      | None -> mlir_context_create_with_threading threading


    let equal = mlir_context_equal
    let destroy = mlir_context_destroy
    let allow_unregistered_dialects = mlir_context_set_allow_unregistered_dialects
    let allows_unregistered_dialects = mlir_context_get_allow_unregistered_dialects

    let num_registered_dialects =
      mlir_context_get_num_registered_dialects >> Intptr.to_int


    let append_dialect_registry = mlir_context_append_dialect_registry
    let num_loaded_dialects = mlir_context_get_num_loaded_dialects >> Intptr.to_int

    let get_or_load_dialect context name =
      mlir_context_get_or_load_dialect context (mlir_string_ref_create_from_cstring name)


    let enable_multithreading = mlir_context_enable_multithreading
    let load_all_available_dialects = mlir_context_load_all_available_dialects

    let is_registered_operation ctx name =
      mlir_string_ref_create_from_cstring name |> mlir_context_is_registered_operation ctx


    let set_thread_pool = mlir_context_set_thread_pool
  end

  module Dialect = struct
    type t = MlirDialect.t structure

    let context = mlir_dialect_get_context
    let equal = mlir_dialect_equal
    let namespace dialect = mlir_dialect_get_namespace dialect |> as_string
  end

  module DialectHandle = struct
    type t = MlirDialectHandle.t structure

    let arith = mlir_get_dialect_handle__arith_
    let func = mlir_get_dialect_handle__func_
    let linalg = mlir_get_dialect_handle__linalg_
    let memref = mlir_get_dialect_handle__memref_
    let scf = mlir_get_dialect_handle__scf_
    let sparse_tensor = mlir_get_dialect_handle__sparse_tensor_
    let tensor = mlir_get_dialect_handle__tensor_
    let transform = mlir_get_dialect_handle__transform_
    let namespace = mlir_dialect_handle_get_namespace >> as_string
    let insert_dialect = mlir_dialect_handle_insert_dialect
    let register_dialect = mlir_dialect_handle_register_dialect
    let load_dialect = mlir_dialect_handle_load_dialect
  end

  module DialectRegistry = struct
    type t = MlirDialectRegistry.t structure

    let create = mlir_dialect_registry_create
    let destroy = mlir_dialect_registry_destroy
  end

  module Type = struct
    type t = MlirType.t structure

    let parse ctx = mlir_string_ref_create_from_cstring >> mlir_type_parse_get ctx
    let bf16 = mlir_bf16_type_get
    let f16 = mlir_f16_type_get
    let f32 = mlir_f32_type_get
    let f64 = mlir_f64_type_get
    let index = mlir_index_type_get
    let none = mlir_none_type_get
    let context = mlir_type_get_context
    let type_id = mlir_type_get_type_id
    let dialect = mlir_type_get_dialect
    let equal = mlir_type_equal

    let print t ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_type_print t funptr null


    let dump = mlir_type_dump
  end

  module Identifier = struct
    type t = MlirIdentifier.t structure

    let from ctx = mlir_string_ref_create_from_cstring >> mlir_identifier_get ctx
    let context = mlir_identifier_get_context
    let equal = mlir_identifier_equal
    let str = mlir_identifier_str >> as_string
  end

  module Attribute = struct
    type t = MlirAttribute.t structure

    let parse ctx = mlir_string_ref_create_from_cstring >> mlir_attribute_parse_get ctx
    let unit = mlir_unit_attr_get
    let context = mlir_attribute_get_context
    let get_type = mlir_attribute_get_type
    let type_id = mlir_attribute_get_type_id
    let dialect = mlir_attribute_get_dialect
    let equal = mlir_attribute_equal

    let print attribute ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_attribute_print attribute funptr null


    let dump = mlir_attribute_dump
  end

  module Location = struct
    type t = MlirLocation.t structure

    let attribute = mlir_location_from_attribute

    let file_line_col ctx filename line col =
      mlir_location_file_line_col_get
        ctx
        (mlir_string_ref_create_from_cstring filename)
        (Unsigned.UInt.of_int line)
        (Unsigned.UInt.of_int col)


    let call_site = mlir_location_call_site_get

    let fused ctx locations =
      let loc_array = CArray.of_list MlirLocation.t locations in
      mlir_location_fused_get
        ctx
        (CArray.length loc_array |> Intptr.of_int)
        (CArray.start loc_array)


    let name ctx name location_opt =
      let loc =
        Option.value
          location_opt
          ~default:
            (let new_loc = make MlirLocation.t in
             setf new_loc MlirLocation.ptr null;
             new_loc)
      in
      mlir_location_name_get ctx (mlir_string_ref_create_from_cstring name) loc


    let unknown = mlir_location_unknown_get
    let get_attribute = mlir_location_get_attribute
    let context = mlir_location_get_context
    let equal = mlir_location_equal

    let print location ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_location_print location funptr null
  end

  module OpPrintingFlags = struct
    type t = MlirOpPrintingFlags.t structure

    let create = mlir_op_printing_flags_create
    let destroy = mlir_op_printing_flags_destroy

    let elide_large_elements_attributes flags limit =
      mlir_op_printing_flags_elide_large_elements_attrs flags (Intptr.of_int limit);
      flags


    let elide_large_resource_string flags limit =
      mlir_op_printing_flags_elide_large_resource_string flags (Intptr.of_int limit);
      flags


    let enable_debug_info flags enable pretty =
      mlir_op_printing_flags_enable_debug_info flags enable pretty;
      flags


    let print_generic_op_form flags =
      mlir_op_printing_flags_print_generic_op_form flags;
      flags


    let use_local_scope flags =
      mlir_op_printing_flags_use_local_scope flags;
      flags


    let skip_regions flags =
      mlir_op_printing_flags_skip_regions flags;
      flags
  end

  module BytecodeWriterConfig = struct
    type t = MlirBytecodeWriterConfig.t structure

    let create = mlir_bytecode_writer_config_create
    let destroy = mlir_bytecode_writer_config_destroy

    let desired_emit_version config version =
      mlir_bytecode_writer_config_desired_emit_version config (Int64.of_int version)
  end

  module AsmState = struct
    type t = MlirAsmState.t structure

    let create_for_operation = mlir_asm_state_create_for_operation
    let create_for_value = mlir_asm_state_create_for_value
    let destroy = mlir_asm_state_destroy
  end

  module Operation = struct
    type t = MlirOperation.t structure

    let parse ctx source name =
      mlir_operation_create_parse
        ctx
        (mlir_string_ref_create_from_cstring source)
        (mlir_string_ref_create_from_cstring name)


    let clone = mlir_operation_clone
    let destroy = mlir_operation_destroy
    let remove_from_parent = mlir_operation_remove_from_parent
    let equal = mlir_operation_equal
    let context = mlir_operation_get_context
    let location = mlir_operation_get_location
    let type_id = mlir_operation_get_type_id
    let name = mlir_operation_get_name
    let block = mlir_operation_get_block
    let next = mlir_operation_get_next_in_block

    let parent operation =
      let result = mlir_operation_get_parent_operation operation in
      if mlir_operation_is_null result then None else Some result


    let num_regions = mlir_operation_get_num_regions >> Intptr.to_int
    let first_region = mlir_operation_get_first_region

    module Regions = struct
      type t = MlirOperation.t structure
      type elt = MlirRegion.t structure

      let get operation index = mlir_operation_get_region operation (Intptr.of_int index)
      let size = mlir_operation_get_num_regions >> Intptr.to_int
    end

    let regions (operation : t) = operation

    module Operands = struct
      type t = MlirOperation.t structure
      type elt = MlirValue.t structure

      let size = mlir_operation_get_num_operands >> Intptr.to_int
      let get operation index = mlir_operation_get_operand operation (Intptr.of_int index)
      let set operation index = mlir_operation_set_operand operation (Intptr.of_int index)

      let replace operation operands =
        if List.length operands != size operation
        then
          raise
            (OutOfViewBounds
               "Replacing old operands with a different number new operands!")
        else (
          let operand_array = CArray.of_list MlirValue.t operands in
          mlir_operation_set_operands
            operation
            (CArray.length operand_array |> Intptr.of_int)
            (CArray.start operand_array))
    end

    let operands (operation : t) = operation

    module Results = struct
      type t = MlirOperation.t structure
      type elt = MlirValue.t structure

      let size = mlir_operation_get_num_results >> Intptr.to_int
      let get operation index = mlir_operation_get_result operation (Intptr.of_int index)
    end

    let results (operation : t) = operation

    module Successors = struct
      type t = MlirOperation.t structure
      type elt = MlirBlock.t structure

      let size = mlir_operation_get_num_successors >> Intptr.to_int

      let get operation index =
        mlir_operation_get_successor operation (Intptr.of_int index)


      let set operation index =
        mlir_operation_set_successor operation (Intptr.of_int index)


      let replace operation operands =
        if List.length operands != size operation
        then
          raise
            (OutOfViewBounds
               "Replacing old operands with a different number new operands!")
        else List.iteri (fun index operand -> set operation index operand) operands
    end

    let successors (operation : t) = operation

    module InheritedAttributes = struct
      type t = MlirOperation.t structure
      type value = MlirAttribute.t structure

      let contains operation name =
        mlir_operation_has_inherent_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)


      let lookup operation name =
        mlir_operation_get_inherent_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)


      let put operation name =
        mlir_operation_set_inherent_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)


      let remove _ _ = false (* TODO: check if passing a null attribute does the trick *)
    end

    let inherited_attributes (operation : t) = operation

    module DiscardableAttributes = struct
      type t = MlirOperation.t structure
      type elt = Identifier.t * Attribute.t
      type value = MlirAttribute.t structure

      let size = mlir_operation_get_num_discardable_attributes >> Intptr.to_int

      let get operation index =
        let result =
          mlir_operation_get_discardable_attribute operation (Intptr.of_int index)
        in
        getf result MlirNamedAttribute.name, getf result MlirNamedAttribute.attribute


      let contains operation name =
        let result =
          mlir_operation_get_discardable_attribute_by_name
            operation
            (mlir_string_ref_create_from_cstring name)
        in
        if mlir_attribute_is_null result then false else true


      let lookup operation name =
        mlir_operation_get_discardable_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)


      let put operation name attribute =
        mlir_operation_set_discardable_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)
          attribute


      let remove operation name =
        mlir_operation_remove_discardable_attribute_by_name
          operation
          (mlir_string_ref_create_from_cstring name)
    end

    let discardable_attributes (operation : t) = operation

    let print operation ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_operation_print operation funptr null


    let print_with_flags operation flags ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_operation_print_with_flags operation flags funptr null


    let print_with_state operation state ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_operation_print_with_state operation state funptr null


    let write_bytecode operation ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_operation_write_bytecode operation funptr null


    let write_bytecode_with_config operation config ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_operation_write_bytecode_with_config operation config funptr null
      |> Support.LogicalResult.from_raw


    let dump = mlir_operation_dump
    let verify = mlir_operation_verify
    let move_after = mlir_operation_move_after
    let move_before = mlir_operation_move_before

    type walk_result =
      | Advance
      | Interrupt
      | Skip

    type walk_order =
      | PreOrder
      | PostOrder

    let walk operation ~callback order =
      let funptr =
        coerce
          (Foreign.funptr (MlirOperation.t @-> ptr void @-> returning MlirWalkResult.t))
          MlirOperationWalkCallback.t
          (fun op _ ->
             match callback op with
             | Advance -> MlirWalkResultAdvance
             | Interrupt -> MlirWalkResultInterrupt
             | Skip -> MlirWalkResultSkip)
      in
      mlir_operation_walk
        operation
        funptr
        null
        (match order with
         | PreOrder -> MlirWalkPreOrder
         | PostOrder -> MlirWalkPostOrder)
  end

  module Region = struct
    type t = MlirRegion.t structure

    let create = mlir_region_create
    let destroy = mlir_region_destroy
    let equal = mlir_region_equal
    let first = mlir_region_get_first_block
    let append_owned_block = mlir_region_append_owned_block

    let insert_owned_block region index =
      mlir_region_insert_owned_block region (Intptr.of_int index)


    let insert_owned_block_after = mlir_region_insert_owned_block_after
    let insert_owned_block_before = mlir_region_insert_owned_block_before
    let next = mlir_region_get_next_in_operation
    let take_body = mlir_region_take_body
  end

  module Block = struct
    type t = MlirBlock.t structure

    let create types locations =
      if List.length types != List.length locations
      then
        raise (Error "Provided different number of types and locations, must be the same")
      else (
        let type_array = CArray.of_list MlirType.t types in
        let loc_array = CArray.of_list MlirLocation.t locations in
        mlir_block_create
          (List.length types |> Intptr.of_int)
          (CArray.start type_array)
          (CArray.start loc_array))


    let destroy = mlir_block_destroy
    let detach = mlir_block_detach
    let equal = mlir_block_equal
    let parent_operation = mlir_block_get_parent_operation
    let parent_region = mlir_block_get_parent_region
    let next = mlir_block_get_next_in_region
    let first = mlir_block_get_first_operation
    let terminator = mlir_block_get_terminator
    let append_owned_operation = mlir_block_append_owned_operation

    let insert_owned_operation block index =
      mlir_block_insert_owned_operation block (Intptr.of_int index)


    let insert_owned_operation_after region reference_opt =
      let reference =
        Option.value
          reference_opt
          ~default:
            (let new_ref = make MlirOperation.t in
             setf new_ref MlirOperation.ptr null;
             new_ref)
      in
      mlir_block_insert_owned_operation_after region reference


    let insert_owned_operation_before region reference_opt =
      let reference =
        Option.value
          reference_opt
          ~default:
            (let new_ref = make MlirOperation.t in
             setf new_ref MlirOperation.ptr null;
             new_ref)
      in
      mlir_block_insert_owned_operation_before region reference


    (* explicit checks to BlockArgument not necessary *)
    module BlockArguments = struct
      type t = MlirBlock.t structure

      let size = mlir_block_get_num_arguments >> Intptr.to_int
      let add = mlir_block_add_argument
      let erase block index = mlir_block_erase_argument block (Unsigned.UInt.of_int index)
      let insert block index = mlir_block_insert_argument block (Intptr.of_int index)
      let get block index = mlir_block_get_argument block (Intptr.of_int index)
    end

    let arguments (block : t) = block

    let print block ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_block_print block funptr null
  end

  module OpOperand = struct
    type t = MlirOpOperand.t structure

    let value = mlir_op_operand_get_value
    let owner = mlir_op_operand_get_owner
    let number = mlir_op_operand_get_operand_number >> Unsigned.UInt.to_int

    let next_use operand =
      let result = mlir_op_operand_get_next_use operand in
      if mlir_op_operand_is_null result then None else Some result
  end

  module Value = struct
    type t = MlirValue.t structure

    let equal = mlir_value_equal
    let is_block_argument = mlir_value_is_ablock_argument
    let is_op_result = mlir_value_is_aop_result
    let get_type = mlir_value_get_type
    let set_type = mlir_value_set_type

    let print value ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_value_print value funptr null


    let print_as_operand value state ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
          MlirStringCallback.t
          (fun s _ -> callback (as_string s))
      in
      mlir_value_print_as_operand value state funptr null


    let dump = mlir_value_dump

    let first_use value =
      let result = mlir_value_get_first_use value in
      if mlir_op_operand_is_null result then None else Some result


    let replace_all_uses_of_with = mlir_value_replace_all_uses_of_with
  end

  module BlockArgument = struct
    include Value

    let from value = if Value.is_block_argument value then Some value else None
    let owner = mlir_block_argument_get_owner
    let number = mlir_block_argument_get_arg_number >> Intptr.to_int
    let set_type = mlir_block_argument_set_type
  end

  module OpResult = struct
    include Value

    let from value = if Value.is_op_result value then Some value else None
    let owner = mlir_op_result_get_owner
    let number = mlir_op_result_get_result_number >> Intptr.to_int
  end

  module Module = struct
    type t = MlirModule.t structure

    let create = mlir_module_create_empty
    let parse ctx = mlir_string_ref_create_from_cstring >> mlir_module_create_parse ctx
    let context = mlir_module_get_context
    let body = mlir_module_get_body
    let destroy = mlir_module_destroy
    let to_operation = mlir_module_get_operation

    let from_operation operation =
      let result = mlir_module_from_operation operation in
      if mlir_module_is_null result then None else Some result
  end

  module SymbolTable = struct
    type t = MlirSymbolTable.t structure

    let symbol_attribute_name = mlir_symbol_table_get_symbol_attribute_name >> as_string

    let visibility_attribute_name =
      mlir_symbol_table_get_visibility_attribute_name >> as_string


    let create operation =
      let result = mlir_symbol_table_create operation in
      if mlir_symbol_table_is_null result then None else Some result


    let destroy = mlir_symbol_table_destroy

    let lookup table name =
      let result =
        mlir_symbol_table_lookup table (mlir_string_ref_create_from_cstring name)
      in
      if mlir_operation_is_null result then None else Some result


    let insert = mlir_symbol_table_insert
    let erase = mlir_symbol_table_erase

    let replace_all_symbol_uses old_symbol new_symbol =
      mlir_symbol_table_replace_all_symbol_uses
        (mlir_string_ref_create_from_cstring old_symbol)
        (mlir_string_ref_create_from_cstring new_symbol)
      >> Support.LogicalResult.from_raw


    let walk operation all_sym_uses_visible ~callback =
      let funptr =
        coerce
          (Foreign.funptr (MlirOperation.t @-> bool @-> ptr void @-> returning void))
          (static_funptr (MlirOperation.t @-> bool @-> ptr void @-> returning void))
          (fun op flag _ -> callback op flag)
      in
      mlir_symbol_table_walk_symbol_tables operation all_sym_uses_visible funptr null
  end

  module OpBuilder = struct
    type t = MlirOperationState.t structure ptr

    let create name location =
      allocate
        MlirOperationState.t
        (mlir_operation_state_get (mlir_string_ref_create_from_cstring name) location)


    let destroy = to_voidp >> Root.release

    let add_results builder types =
      let type_array = CArray.of_list MlirType.t types in
      mlir_operation_state_add_results
        builder
        (List.length types |> Intptr.of_int)
        (CArray.start type_array);
      builder


    let add_operands builder values =
      let value_array = CArray.of_list MlirValue.t values in
      mlir_operation_state_add_operands
        builder
        (List.length values |> Intptr.of_int)
        (CArray.start value_array);
      builder


    let add_owned_regions builder regions =
      let region_array = CArray.of_list MlirRegion.t regions in
      mlir_operation_state_add_owned_regions
        builder
        (List.length regions |> Intptr.of_int)
        (CArray.start region_array);
      builder


    let add_successors builder blocks =
      let block_array = CArray.of_list MlirBlock.t blocks in
      mlir_operation_state_add_successors
        builder
        (List.length blocks |> Intptr.of_int)
        (CArray.start block_array);
      builder


    let add_attributes builder attributes =
      let attribute_array =
        CArray.of_list
          MlirNamedAttribute.t
          (List.map (fun (id, attr) -> mlir_named_attribute_get id attr) attributes)
      in
      mlir_operation_state_add_attributes
        builder
        (List.length attributes |> Intptr.of_int)
        (CArray.start attribute_array);
      builder


    let enable_result_type_inference builder =
      mlir_operation_state_enable_result_type_inference builder;
      builder


    let build builder =
      let result = mlir_operation_create builder in
      if mlir_operation_is_null result
      then raise (Error "Unable to build the operation")
      else if mlir_operation_verify result |> not
      then raise (Error "Unable to verify the operation")
      else result
  end

  module type AttributeLike = sig
    type t

    val context : t -> Context.t
    val get_type : t -> Type.t
    val type_id : t -> Support.TypeId.t
    val dialect : t -> Dialect.t
    val equal : t -> t -> bool
    val print : t -> callback:(string -> unit) -> unit
    val dump : t -> unit
  end

  module type OperationLike = sig
    type t

    val clone : t -> t
    val destroy : t -> unit
    val remove_from_parent : t -> unit
    val equal : t -> t -> bool
    val context : t -> Context.t
    val location : t -> Location.t
    val type_id : t -> Support.TypeId.t
    val name : t -> Identifier.t
    val block : t -> Block.t
    val next : t -> t
    val parent : t -> t option
    val num_regions : t -> int
    val first_region : t -> Region.t

    module Regions : sig
      type t

      include ListView with type t := t with type elt = Region.t
    end

    val regions : t -> Regions.t

    module Operands : sig
      type t

      include ArrayView with type t := t with type elt = Value.t
    end

    val operands : t -> Operands.t

    module Results : sig
      type t

      include ListView with type t := t with type elt = OpResult.t
    end

    val results : t -> Results.t

    module Successors : sig
      type t

      include ArrayView with type t := t with type elt = Block.t
    end

    val successors : t -> Successors.t

    module InheritedAttributes : sig
      type t

      include MapView with type t := t with type value = Attribute.t
    end

    val inherited_attributes : t -> InheritedAttributes.t

    module DiscardableAttributes : sig
      type t

      include ListView with type t := t with type elt = Identifier.t * Attribute.t
      include MapView with type t := t with type value = Attribute.t
    end

    val discardable_attributes : t -> DiscardableAttributes.t
    val print : t -> callback:(string -> unit) -> unit
    val print_with_flags : t -> OpPrintingFlags.t -> callback:(string -> unit) -> unit
    val print_with_state : t -> AsmState.t -> callback:(string -> unit) -> unit
    val write_bytecode : t -> callback:(string -> unit) -> unit

    val write_bytecode_with_config
      :  t
      -> BytecodeWriterConfig.t
      -> callback:(string -> unit)
      -> Support.LogicalResult.t

    val dump : t -> unit
    val verify : t -> bool
    val move_after : t -> t -> unit
    val move_before : t -> t -> unit

    type walk_result =
      | Advance
      | Interrupt
      | Skip

    type walk_order =
      | PreOrder
      | PostOrder

    val walk : t -> callback:(t -> walk_result) -> walk_order -> unit
  end

  module type TypeLike = sig
    type t

    val context : t -> Context.t
    val type_id : t -> Support.TypeId.t
    val dialect : t -> Dialect.t
    val equal : t -> t -> bool
    val print : t -> callback:(string -> unit) -> unit
    val dump : t -> unit
  end

  module type ValueLike = sig
    type t

    val equal : t -> t -> bool
    val get_type : t -> Type.t
    val set_type : t -> Type.t -> unit
    val print : t -> callback:(string -> unit) -> unit
    val print_as_operand : t -> AsmState.t -> callback:(string -> unit) -> unit
    val dump : t -> unit
    val first_use : t -> OpOperand.t option
    val replace_all_uses_of_with : t -> t -> unit
  end
end

let with_context ctx f =
  let result = f ctx in
  Ir.Context.destroy ctx;
  result


let register_all_dialects = mlir_register_all_dialects
let register_all_llvm_translations = mlir_register_all_llvmtranslations
let register_all_passes = mlir_register_all_passes

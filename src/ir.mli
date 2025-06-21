open Ctypes
open Bindings.Types
open Support.Support

module rec Ir : sig
  module Context : sig
    class type t = object
      method allows_unregistered_dialects : bool
      method allow_unregistered_dialects : bool -> unit
      method registered_dialects : int
      method register_dialect : Ir.DialectHandle.t -> unit
      method loaded_dialects : int
      method append_dialect_registry : Ir.DialectRegistry.t -> unit
      method enable_multithreading : bool -> unit
      method load_all_available_dialects : unit
      method is_registered_operation : string -> bool
      method set_thread_pool : LLVMThreadPool.t -> unit
      method destroy : unit
      method raw : MlirContext.t structure
    end

    val from_raw : MlirContext.t structure -> t
    val get : Ir.DialectRegistry.t option -> bool -> t
    val equal : t -> t -> bool
  end

  module Dialect : sig
    class type t = object
      method context : Context.t
      method namespace : string
      method raw : MlirDialect.t structure
    end

    val from_raw : MlirDialect.t structure -> t
    val load : Context.t -> Ir.DialectHandle.t -> t
    val get_or_load : Context.t -> string -> t
    val equal : t -> t -> bool
  end

  module DialectHandle : sig
    class type t = object
      method namespace : string
      method raw : MlirDialectHandle.t structure
    end

    val from_raw : MlirDialectHandle.t structure -> t
    val arith : unit -> t
    val func : unit -> t
    val linalg : unit -> t
    val memref : unit -> t
    val scf : unit -> t
    val sparse_tensor : unit -> t
    val tensor : unit -> t
    val transform : unit -> t
  end

  module DialectRegistry : sig
    class type t = object
      method insert_dialect : DialectHandle.t -> unit
      method destroy : unit
      method raw : MlirDialectRegistry.t structure
    end

    val from_raw : MlirDialectRegistry.t structure -> t
    val get : unit -> t
  end

  module Type : sig
    class type t = object
      method context : Context.t
      method id : TypeId.t
      method dialect : Dialect.t
      method raw : MlirType.t structure
    end

    val from_raw : MlirType.t structure -> t
    val parse : Context.t -> string -> t
    val bfloat16 : Context.t -> t
    val float16 : Context.t -> t
    val float32 : Context.t -> t
    val float64 : Context.t -> t
    val index : Context.t -> t
    val none : Context.t -> t
    val is_shaped : #t -> bool
    val equal : #t -> #t -> bool
    val print : callback:(string -> unit) -> #t -> unit
    val dump : #t -> unit
  end

  module Identifier : sig
    class type t = object
      method context : Context.t
      method str : string
      method raw : MlirIdentifier.t structure
    end

    val from_raw : MlirIdentifier.t structure -> t
    val get : Context.t -> string -> t
    val equal : t -> t -> bool
  end

  module Attribute : sig
    class type t = object
      method context : Context.t
      method t : Type.t
      method id : TypeId.t
      method dialect : Dialect.t
      method raw : MlirAttribute.t structure
    end

    val from_raw : MlirAttribute.t structure -> t
    val parse : Context.t -> string -> t

    (** [is_affine_map] checks whether the given attribute is an affine map attribute *)
    val is_affine_map : #t -> bool

    (** [is_array attr] checks whether the given attribute is an array attribute *)
    val is_array : #t -> bool

    (** [is_typed attr] checks whether the given [attr] is a type attribute *)
    val is_typed : #t -> bool

    (** [is_float attr] checks whether the given [attr] is a floating point attribute *)
    val is_float : #t -> bool

    (** [is_string attr] checks whether the given [attr] is a string attribute *)
    val is_string : #t -> bool

    (** [is_dense_bool_array attr] checks whether the given attribute is a dense boolean array attribute *)
    val is_dense_bool_array : #t -> bool

    (** [is_dense_int8_array attr] checks whether the given attribute is a dense 8-bit integer array attribute *)
    val is_dense_int8_array : #t -> bool

    (** [is_dense_int16_array attr] checks whether the given attribute is a dense 16-bit integer array attribute *)
    val is_dense_int16_array : #t -> bool

    (** [is_dense_int32_array attr] checks whether the given attribute is a dense 32-bit integer array attribute *)
    val is_dense_int32_array : #t -> bool

    (** [is_dense_int64_array attr] checks whether the given attribute is a dense 64-bit integer array attribute *)
    val is_dense_int64_array : #t -> bool

    val unit : Context.t -> t
    val equal : #t -> #t -> bool
    val print : callback:(string -> unit) -> #t -> unit
    val dump : #t -> unit
  end

  module Location : sig
    class type t = object
      method attribute : Attribute.t
      method context : Context.t
      method raw : MlirLocation.t structure
    end

    val attribute : Attribute.t -> t
    val file_line_col : Context.t -> string -> int -> int -> t
    val call_site : t -> t -> t
    val fused : Context.t -> t list -> Attribute.t -> t
    val name : Context.t -> string -> t option -> t
    val unknown : Context.t -> t
    val equal : t -> t -> bool
    val print : callback:(string -> unit) -> t -> unit
  end

  module OpPrintingFlags : sig
    class type t = object
      method destroy : unit
      method raw : MlirOpPrintingFlags.t structure
    end

    val from_raw : MlirOpPrintingFlags.t structure -> t
    val get : unit -> t
    val elide_large_elements_attributes : int -> t -> t
    val elide_large_resource_string : int -> t -> t
    val enable_debug_info : bool -> bool -> t -> t
    val print_generic_op_form : t -> t
    val use_local_scope : t -> t
    val skip_regions : t -> t
  end

  module BytecodeWriterConfig : sig
    class type t = object
      method desired_emit_version : int -> unit
      method destroy : unit
      method raw : MlirBytecodeWriterConfig.t structure
    end

    val from_raw : MlirBytecodeWriterConfig.t structure -> t
    val get : unit -> t
  end

  module AsmState : sig
    class type t = object
      method destroy : unit
      method raw : MlirAsmState.t structure
    end

    val from_raw : MlirAsmState.t structure -> t
    val create_for_operation : Ir.Operation.t -> OpPrintingFlags.t -> t
    val create_for_value : Ir.Value.t -> OpPrintingFlags.t -> t
  end

  module Value : sig
    class type t = object
      method get_type : Type.t
      method set_type : #Type.t -> unit
      method first_use : Ir.OpOperand.t option
      method raw : MlirValue.t structure
    end

    val from_raw : MlirValue.t structure -> t

    (* val is_block_argument : t -> bool
    val is_op_result : t -> bool *)
    val replace_all_uses_of_with : #t -> #t -> unit
    val equal : #t -> #t -> bool
    val print : callback:(string -> unit) -> #t -> unit
    val print_as_operand : AsmState.t -> callback:(string -> unit) -> #t -> unit
    val dump : #t -> unit
  end

  module OpOperand : sig
    class type t = object
      method value : Value.t
      method owner : Ir.Operation.t
      method number : int
      method next_use : t option
      method raw : MlirOpOperand.t structure
    end

    val from_raw : MlirOpOperand.t structure -> t
  end

  module BlockArgument : sig
    class type t = object
      inherit Value.t
      method owner : Ir.Block.t
      method number : int
    end

    val from_raw : MlirValue.t structure -> t
  end

  module OpResult : sig
    class type t = object
      inherit Value.t
      method owner : Ir.Operation.t
      method number : int
    end

    val from_raw : MlirValue.t structure -> t
  end

  module Operation : sig
    class type t = object
      method context : Context.t
      method location : Location.t
      method id : TypeId.t option
      method name : Identifier.t
      method block : Ir.Block.t option
      method next : t option
      method parent : t option
      method first_region : Ir.Region.t option
      method regions : int
      method region : int -> Ir.Region.t
      method iter_regions : f:(Ir.Region.t -> unit) -> unit
      method iteri_regions : f:(int -> Ir.Region.t -> unit) -> unit
      method operands : int
      method operand : int -> Value.t
      method set_operand : int -> #Value.t -> unit
      method iter_operands : f:(Value.t -> unit) -> unit
      method iteri_operands : f:(int -> Value.t -> unit) -> unit
      method replace_operands : #Value.t list -> unit
      method results : int
      method result : int -> OpResult.t
      method iter_results : f:(OpResult.t -> unit) -> unit
      method iteri_results : f:(int -> OpResult.t -> unit) -> unit
      method successors : int
      method successor : int -> Ir.Block.t
      method set_successor : int -> Ir.Block.t -> unit
      method iter_successors : f:(Ir.Block.t -> unit) -> unit
      method iteri_successors : f:(int -> Ir.Block.t -> unit) -> unit
      method has_inherent_attribute : string -> bool
      method inherent_attribute : string -> Attribute.t option
      method set_inherent_attribute : string -> #Attribute.t -> unit
      method discardable_attributes : int
      method discardable_attribute : string -> Attribute.t option
      method put_discardable_attribute : string -> #Attribute.t -> unit
      method discardable_attribute_at : int -> Identifier.t * Attribute.t
      method remove_discardable_attribute : string -> bool
      method iter_discardable_attributes : f:(Identifier.t * Attribute.t -> unit) -> unit

      method iteri_discardable_attributes :
        f:(int -> Identifier.t * Attribute.t -> unit) -> unit

      method verify : bool
      method move_after : t -> unit
      method move_before : t -> unit
      method clone : t
      method remove_from_parent : unit
      method destroy : unit
      method raw : MlirOperation.t structure
    end

    val from_raw : MlirOperation.t structure -> t
    val parse : Context.t -> string -> string -> t
    val equal : t -> t -> bool
    val print : callback:(string -> unit) -> t -> unit
    val print_with_flags : OpPrintingFlags.t -> callback:(string -> unit) -> t -> unit
    val print_with_state : AsmState.t -> callback:(string -> unit) -> t -> unit
    val write_bytecode : callback:(string -> unit) -> t -> unit

    val write_bytecode_with_config
      :  BytecodeWriterConfig.t
      -> callback:(string -> unit)
      -> t
      -> LogicalResult.t

    val dump : t -> unit

    type walk_result =
      | Advance
      | Interrupt
      | Skip

    type walk_order =
      | PreOrder
      | PostOrder

    val walk : callback:(t -> walk_result) -> walk_order -> t -> unit
  end

  module Block : sig
    class type t = object
      method parent_operation : Operation.t option
      method parent_region : Ir.Region.t
      method next : t option
      method first : Operation.t option
      method terminator : Operation.t option
      method append_operation : Operation.t -> unit
      method insert_operation : int -> Operation.t -> unit
      method insert_operation_after : Operation.t option -> Operation.t -> unit
      method insert_operation_before : Operation.t option -> Operation.t -> unit
      method iter_operations : f:(Operation.t -> unit) -> unit
      method iteri_operations : f:(int -> Operation.t -> unit) -> unit
      method arguments : int
      method argument : int -> BlockArgument.t
      method add_argument : #Type.t -> Location.t -> BlockArgument.t
      method insert_argument : int -> #Type.t -> Location.t -> BlockArgument.t
      method erase_argument : int -> unit
      method iter_arguments : f:(BlockArgument.t -> unit) -> unit
      method iteri_arguments : f:(int -> BlockArgument.t -> unit) -> unit
      method detach : unit
      method destroy : unit
      method raw : MlirBlock.t structure
    end

    val from_raw : MlirBlock.t structure -> t
    val get : (#Type.t * Location.t) list -> t
    val equal : t -> t -> bool
    val print : callback:(string -> unit) -> t -> unit
  end

  module Region : sig
    class type t = object
      method first : Block.t option
      method append_block : Block.t -> unit
      method insert_block : int -> Block.t -> unit
      method insert_block_after : Block.t option -> Block.t -> unit
      method insert_block_before : Block.t option -> Block.t -> unit
      method iter_blocks : f:(Block.t -> unit) -> unit
      method iteri_blocks : f:(int -> Block.t -> unit) -> unit
      method next : t option
      method take_body : t -> unit
      method destroy : unit
      method raw : MlirRegion.t structure
    end

    val from_raw : MlirRegion.t structure -> t
    val get : unit -> t
    val equal : t -> t -> bool
  end

  module Module : sig
    class type t = object
      method context : Context.t
      method body : Block.t
      method to_operation : Operation.t
      method destroy : unit
      method raw : MlirModule.t structure
    end

    val from_raw : MlirModule.t structure -> t
    val empty : Location.t -> t
    val parse : Context.t -> string -> t
    val from_operation : Operation.t -> t option
  end

  module SymbolTable : sig
    class type t = object
      method lookup : string -> Operation.t option
      method insert : Operation.t -> Attribute.t
      method erase : Operation.t -> unit
      method destroy : unit
      method raw : MlirSymbolTable.t structure
    end

    val from_raw : MlirSymbolTable.t structure -> t
    val symbol_attribute_name : unit -> string
    val visibility_attribute_name : unit -> string
    val get : Operation.t -> t option
    val replace_all_symbol_uses : string -> string -> Operation.t -> LogicalResult.t
    val walk : bool -> callback:(Operation.t -> bool -> unit) -> Operation.t -> unit
  end

  module OpBuilder : sig
    type t

    val get : string -> Location.t -> t
    val destroy : t -> unit
    val add_results : #Type.t list -> t -> t
    val add_operands : #Value.t list -> t -> t
    val add_regions : Region.t list -> t -> t
    val add_successors : Block.t list -> t -> t
    val add_attributes : (Identifier.t * #Attribute.t) list -> t -> t
    val enable_result_type_inference : t -> t
    val build : bool -> t -> Operation.t
  end
end

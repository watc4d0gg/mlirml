open Ctypes
open Unsigned
open Utils

(* Support *)

exception Error of string

module Support : sig
  module LogicalResult : sig
    type t =
      | Success
      | Failure
  end

  module LLVMThreadPool : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
  end

  module TypeId : sig
    type t

    val create : 'a -> 'a typ -> t
    val equal : t -> t -> bool
    val hash : t -> size_t
  end

  module TypeIdAllocator : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val allocated_type_id : t -> TypeId.t
  end
end

(* IR *)

(* TODO: add optional result types where necessary *)

module rec Ir : sig
  module Context : sig
    type t

    val create : Ir.DialectRegistry.t option -> bool -> t
    val equal : t -> t -> bool
    val destroy : t -> unit
    val allow_unregistered_dialects : t -> bool -> unit
    val allows_unregistered_dialects : t -> bool
    val num_registered_dialects : t -> int
    val append_dialect_registry : t -> Ir.DialectRegistry.t -> unit
    val num_loaded_dialects : t -> int
    val get_or_load_dialect : t -> string -> Ir.Dialect.t
    val enable_multithreading : t -> bool -> unit
    val load_all_available_dialects : t -> unit
    val is_registered_operation : t -> string -> bool
    val set_thread_pool : t -> Support.LLVMThreadPool.t -> unit
  end

  module Dialect : sig
    type t

    val context : t -> Context.t
    val equal : t -> t -> bool
    val namespace : t -> string
  end

  module DialectRegistry : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
  end

  module DialectHandle : sig
    type t

    val arith : unit -> t
    val func : unit -> t
    val linalg : unit -> t
    val memref : unit -> t
    val scf : unit -> t
    val sparse_tensor : unit -> t
    val tensor : unit -> t
    val transform : unit -> t
    val namespace : t -> string
    val insert_dialect : t -> DialectRegistry.t -> unit
    val register_dialect : t -> Context.t -> unit
    val load_dialect : t -> Context.t -> Dialect.t
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

  module Type : sig
    include TypeLike

    val parse : Context.t -> string -> t
    val bf16 : Context.t -> t
    val f16 : Context.t -> t
    val f32 : Context.t -> t
    val f64 : Context.t -> t
    val index : Context.t -> t
    val none : Context.t -> t
  end

  module Identifier : sig
    type t

    val from : Context.t -> string -> t
    val context : t -> Context.t
    val equal : t -> t -> bool
    val str : t -> string
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

  module Attribute : sig
    include AttributeLike

    val parse : Context.t -> string -> t
    val unit : Context.t -> t
  end

  module Location : sig
    type t

    val attribute : Attribute.t -> t
    val file_line_col : Context.t -> string -> int -> int -> t
    val call_site : t -> t -> t
    val fused : Context.t -> t list -> Attribute.t -> t
    val name : Context.t -> string -> t option -> t
    val unknown : Context.t -> t
    val get_attribute : t -> Attribute.t
    val context : t -> Context.t
    val equal : t -> t -> bool
    val print : t -> callback:(string -> unit) -> unit
  end

  module OpPrintingFlags : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val elide_large_elements_attributes : t -> int -> t
    val elide_large_resource_string : t -> int -> t
    val enable_debug_info : t -> bool -> bool -> t
    val print_generic_op_form : t -> t
    val use_local_scope : t -> t
    val skip_regions : t -> t
  end

  module BytecodeWriterConfig : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val desired_emit_version : t -> int -> unit
  end

  module AsmState : sig
    type t

    val create_for_operation : Ir.Operation.t -> OpPrintingFlags.t -> t
    val create_for_value : Ir.Value.t -> OpPrintingFlags.t -> t
    val destroy : t -> unit
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
    val block : t -> Ir.Block.t
    val next : t -> t
    val parent : t -> t option
    val num_regions : t -> int
    val first_region : t -> Ir.Region.t

    module Regions : sig
      type t

      include ListView with type t := t with type elt = Ir.Region.t
    end

    val regions : t -> Regions.t

    module Operands : sig
      type t

      include ArrayView with type t := t with type elt = Ir.Value.t
    end

    val operands : t -> Operands.t

    module Results : sig
      type t

      include ListView with type t := t with type elt = Ir.OpResult.t
    end

    val results : t -> Results.t

    module Successors : sig
      type t

      include ArrayView with type t := t with type elt = Ir.Block.t
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

  module Operation : sig
    include OperationLike

    val parse : Context.t -> string -> string -> t
  end

  module Region : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val equal : t -> t -> bool
    val first : t -> Ir.Block.t
    val append_owned_block : t -> Ir.Block.t -> unit
    val insert_owned_block : t -> int -> Ir.Block.t -> unit
    val insert_owned_block_after : t -> Ir.Block.t -> Ir.Block.t -> unit
    val insert_owned_block_before : t -> Ir.Block.t -> Ir.Block.t -> unit
    val next : t -> t
    val take_body : t -> t -> unit
  end

  module Block : sig
    type t

    val create : Type.t list -> Location.t list -> t
    val destroy : t -> unit
    val detach : t -> unit
    val equal : t -> t -> bool
    val parent_operation : t -> Operation.t
    val parent_region : t -> Region.t
    val next : t -> t
    val first : t -> Operation.t
    val terminator : t -> Operation.t
    val append_owned_operation : t -> Operation.t -> unit
    val insert_owned_operation : t -> int -> Operation.t -> unit
    val insert_owned_operation_after : t -> Operation.t option -> Operation.t -> unit
    val insert_owned_operation_before : t -> Operation.t option -> Operation.t -> unit

    module BlockArguments : sig
      type t

      val size : t -> int
      val add : t -> Type.t -> Location.t -> Ir.BlockArgument.t
      val erase : t -> int -> unit
      val insert : t -> int -> Type.t -> Location.t -> Ir.BlockArgument.t
      val get : t -> int -> Ir.Value.t
    end

    val arguments : t -> BlockArguments.t
    val print : t -> callback:(string -> unit) -> unit
  end

  module type ValueLike = sig
    type t

    val equal : t -> t -> bool
    val get_type : t -> Type.t
    val set_type : t -> Type.t -> unit
    val print : t -> callback:(string -> unit) -> unit
    val print_as_operand : t -> AsmState.t -> callback:(string -> unit) -> unit
    val dump : t -> unit
    val first_use : t -> Ir.OpOperand.t option
    val replace_all_uses_of_with : t -> t -> unit
  end

  module Value : sig
    include ValueLike

    val is_block_argument : t -> bool
    val is_op_result : t -> bool
  end

  module BlockArgument : sig
    include ValueLike

    val from : Value.t -> t option
    val owner : t -> Block.t
    val number : t -> int
    val set_type : t -> Type.t -> unit
  end

  module OpResult : sig
    include ValueLike

    val from : Value.t -> t option
    val owner : t -> Operation.t
    val number : t -> int
  end

  module OpOperand : sig
    type t

    val value : t -> Value.t
    val owner : t -> Operation.t
    val number : t -> int
    val next_use : t -> t option
  end

  module Module : sig
    type t

    val create : Location.t -> t
    val parse : Context.t -> string -> t
    val context : t -> Context.t
    val body : t -> Block.t
    val destroy : t -> unit
    val to_operation : t -> Operation.t
    val from_operation : Operation.t -> t option
  end

  module SymbolTable : sig
    type t

    val symbol_attribute_name : unit -> string
    val visibility_attribute_name : unit -> string
    val create : Operation.t -> t option
    val destroy : t -> unit
    val lookup : t -> string -> Operation.t option
    val insert : t -> Operation.t -> Attribute.t
    val erase : t -> Operation.t -> unit

    val replace_all_symbol_uses
      :  string
      -> string
      -> Operation.t
      -> Support.LogicalResult.t

    val walk : Operation.t -> bool -> callback:(Operation.t -> bool -> unit) -> unit
  end

  module OpBuilder : sig
    type t

    val create : string -> Location.t -> t
    val destroy : t -> unit
    val add_results : t -> Type.t list -> t
    val add_operands : t -> Value.t list -> t
    val add_owned_regions : t -> Region.t list -> t
    val add_successors : t -> Block.t list -> t
    val add_attributes : t -> (Identifier.t * Attribute.t) list -> t
    val enable_result_type_inference : t -> t
    val build : t -> Operation.t
  end
end

val with_context : Ir.Context.t -> (Ir.Context.t -> 'a) -> 'a
val register_all_dialects : Ir.DialectRegistry.t -> unit
val register_all_llvm_translations : Ir.Context.t -> unit
val register_all_passes : unit -> unit

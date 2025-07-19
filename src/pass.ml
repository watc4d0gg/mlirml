open Ctypes
open Bindings.Types
open Bindings.Functions
open Ir.Ir
open Support
open Utils

module Pass = struct
  type t = MlirPass.t structure
end

module ExternalPass = struct
  class t raw = object (self)
    method signal_pass_failure = mlir_external_pass_signal_failure self#raw
    method raw = raw
  end

  let from_raw = new t

  let create id name argument description op dialects ~on_construct ~on_destruct ~on_clone ~init ~run =
    let dialect_array = dialects |> List.map (fun dialect -> dialect#raw) |> CArray.of_list MlirDialectHandle.t in
    let callbacks = Ctypes.make
      ?finalise:(Some (fun value ->
        let construct_funptr =
          coerce
            (Foreign.funptr (ptr void @-> returning void))
            (static_funptr (ptr void @-> returning void))
            (fun _ -> on_construct ())
        in
        let destruct_funptr =
          coerce
            (Foreign.funptr (ptr void @-> returning void))
            (static_funptr (ptr void @-> returning void))
            (fun _ -> on_destruct ())
        in
        let clone_funptr =
          coerce
            (Foreign.funptr (ptr void @-> returning (ptr void)))
            (static_funptr (ptr void @-> returning (ptr void)))
            (fun _ -> on_clone (); null)
        in
        let init_funptr =
          coerce
            (Foreign.funptr (MlirContext.t @-> ptr void @-> returning MlirLogicalResult.t))
            (static_funptr (ptr MlirContext.t @-> ptr void @-> returning (ptr MlirLogicalResult.t)))
            (fun ctx _ -> init (Context.from_raw ctx))
        in
        let run_funptr =
          coerce
            (Foreign.funptr (MlirOperation.t @-> MlirExternalPass.t @-> ptr void @-> returning void))
            (static_funptr (ptr MlirOperation.t @-> ptr MlirExternalPass.t @-> ptr void @-> returning void))
            (fun op pass _ -> run (Operation.from_raw op) (from_raw pass))
        in
        setf value MlirExternalPassCallbacks.construct construct_funptr;
        setf value MlirExternalPassCallbacks.destruct destruct_funptr;
        setf value MlirExternalPassCallbacks.clone clone_funptr;
        setf value MlirExternalPassCallbacks.initialize init_funptr;
        setf value MlirExternalPassCallbacks.run run_funptr))
        MlirExternalPassCallbacks.t in
    mlir_create_external_pass
      id#raw
      (match name with
      | Some value -> mlir_string_ref_create_from_cstring value
      | None -> mlir_string_ref_create_from_cstring "")
      (mlir_string_ref_create_from_cstring argument)
      (mlir_string_ref_create_from_cstring description)
      (mlir_string_ref_create_from_cstring op)
      (List.length dialects |> Intptr.of_int)
      (CArray.start dialect_array)
      callbacks
      null
end

module OpPassManager = struct
  class t raw = object (self)
    method nested_under = mlir_op_pass_manager_get_nested_under self#raw
    method add_pass pass = mlir_op_pass_manager_add_owned_pass self#raw pass
    method add_pipeline pipeline =
      let error_message = ref String.empty in
      let result = print_raw
        (mlir_op_pass_manager_add_pipeline self#raw)
        ~callback:((^) !error_message >> (:=) error_message)
        (mlir_string_ref_create_from_cstring pipeline) |> LogicalResult.from_raw in
      result, if String.equal !error_message String.empty then None else Some !error_message
    method print_pipeline = print_raw mlir_print_pass_pipeline self#raw
    method parse_pipeline pipeline =
      let error_message = ref String.empty in
      let result = print_raw
        (mlir_parse_pass_pipeline self#raw)
        ~callback:((^) !error_message >> (:=) error_message)
        (mlir_string_ref_create_from_cstring pipeline) |> LogicalResult.from_raw in
      result, if String.equal !error_message String.empty then None else Some !error_message
    method raw = raw
  end
end

module PassManager = struct

  class t raw = object (self)
    method destroy = mlir_pass_manager_destroy self#raw
    method run_on_op (op : Operation.t) = mlir_pass_manager_run_on_op self#raw op#raw |> LogicalResult.from_raw
    method enable_ir_printing print_before_all print_after_all print_module_scope print_after_only_on_change print_after_only_on_failure (flags : OpPrintingFlags.t) tree_printing_path =
      mlir_pass_manager_enable_irprinting self#raw print_before_all print_after_all print_module_scope print_after_only_on_change print_after_only_on_failure flags#raw (mlir_string_ref_create_from_cstring tree_printing_path)
    method enable_verifier enable = mlir_pass_manager_enable_verifier self#raw enable
    method as_op_pass_manager = mlir_pass_manager_get_as_op_pass_manager self#raw
    method nested_under = mlir_pass_manager_get_nested_under self#raw
    method add_pass pass = mlir_pass_manager_add_owned_pass self#raw pass
    method raw = raw
  end

  let from_raw = new t

  let get ctx = mlir_pass_manager_create ctx#raw |> from_raw

  let get_on_operation ctx op = mlir_pass_manager_create_on_operation ctx#raw (mlir_string_ref_create_from_cstring op) |> from_raw

end
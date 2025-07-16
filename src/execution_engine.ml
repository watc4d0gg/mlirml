open Ctypes
open Bindings.Types
open Bindings.Functions
open Utils

module ExecutionEngine = struct
  type argument = Arg : 'a * 'a typ -> argument

  class t raw =
    object (self)
      method lookup name =
        mlir_execution_engine_lookup self#raw (mlir_string_ref_create_from_cstring name)
        |> null_to_opt is_null Fun.id

      method register_symbol : 'a. string -> 'a -> unit =
        fun name value ->
          let allocated = Root.create value in
          mlir_execution_engine_register_symbol
            self#raw
            (mlir_string_ref_create_from_cstring name)
            (to_voidp allocated)

      method destroy = mlir_execution_engine_destroy self#raw

      method dump_to_object_file name =
        mlir_execution_engine_dump_to_object_file
          self#raw
          (mlir_string_ref_create_from_cstring name)

      method raw = raw
    end

  let from_raw = new t

  let get m opt_level shared_lib_paths dump_object_file =
    let lib_array =
      CArray.of_list
        MlirStringRef.t
        (List.map mlir_string_ref_create_from_cstring shared_lib_paths)
    in
    mlir_execution_engine_create
      m#raw
      opt_level
      (CArray.length lib_array)
      (CArray.start lib_array)
      dump_object_file
    |> from_raw
end

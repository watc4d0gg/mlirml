open! Ctypes
open! Bindings.Types
open Bindings.Functions

open Error
open! Ir.Ir
open Utils

(* TODO *)

module SparseTensorEncodingAttr = struct
  class t raw = object (self)
    initializer
      if mlir_attribute_is_asparse_tensor_encoding_attr raw
        then ()
        else
          Error
            ("Unable to cast the attribute "
             ^ print_raw_as_string mlir_attribute_print raw
             ^ " to a SparseTensorEncodingAttr")
          |> raise
    method lvl_rank = mlir_sparse_tensor_encoding_get_lvl_rank self#raw
    method raw = raw
  end
end
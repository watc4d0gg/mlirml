open Mlir

let%test "ctx_equal" = with_context (Context.create ()) (fun ctx -> Context.equal ctx ctx) = true
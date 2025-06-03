open Mlir

let () =
  with_context (Context.create ()) (fun ctx ->
    Bool.to_string (Context.equal ctx ctx) |> print_endline;
    let result = Module.parse ctx "builtin.module {}" in
    Module.destroy result;
    print_endline "Hello, World!")

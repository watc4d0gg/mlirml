let headers = "#include \"mlir-c/IR.h\"\n#include \"mlir-c/Support.h\""

let () =
  let out = open_out "mlir_structs.c" in
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "%s@\n" headers;
  Cstubs_structs.write_c formatter (module Structs.Bindings);
  Format.pp_print_flush formatter ();
  close_out out
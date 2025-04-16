let headers = "#include \"mlir-c/IR.h\"\n#include \"mlir-c/Support.h\""

let () =
  let out_c = open_out "mlir_stubs.c" in
  let out_ml = open_out "mlir_generated.ml" in
  let formatter_c = Format.formatter_of_out_channel out_c in
  let formatter_ml = Format.formatter_of_out_channel out_ml in
  Format.fprintf formatter_c "%s@\n" headers;
  Cstubs.write_c formatter_c ~prefix:"mlir_stub_" (module Stubs.Bindings);
  Cstubs.write_ml formatter_ml ~prefix:"mlir_stub_" (module Stubs.Bindings);
  Format.pp_print_flush formatter_c ();
  Format.pp_print_flush formatter_ml ();
  close_out out_c;
  close_out out_ml
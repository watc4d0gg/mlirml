open Ctypes
open Bindings.Types
open Error

let ( >> ) f g x = f x |> g

let not_null null_check error_message value =
  if null_check value then Error error_message |> raise else value


let null_to_opt null_check map value = if null_check value then None else Some (map value)

let string_ref_as_string string_ref =
  getf string_ref MlirStringRef.data
  |> Ctypes_std_views.char_ptr_of_string
  |> Ctypes.string_from_ptr
       ~length:(getf string_ref MlirStringRef.length |> Unsigned.Size_t.to_int)


let _print_as_string print_fn value =
  let out = ref String.empty in
  print_fn ~callback:(fun result -> out := !out ^ result) value;
  !out


let print_raw print_fn ~callback raw =
  let funptr =
    coerce
      (Foreign.funptr (MlirStringRef.t @-> ptr void @-> returning void))
      MlirStringCallback.t
      (fun s _ -> callback (string_ref_as_string s))
  in
  print_fn raw funptr null


let print_raw_as_string print_fn raw =
  let out = ref String.empty in
  print_raw print_fn ~callback:(fun result -> out := !out ^ result) raw;
  !out

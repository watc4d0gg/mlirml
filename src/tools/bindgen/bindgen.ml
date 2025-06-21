open Clang

type type_info =
  | Enum of bool * string list
  | Struct of bool * (string * string) list
  | TypeDef of string * string

let rec transform_libffi_passable types struct_name =
  match Hashtbl.find types struct_name with
  | Struct (is_typedef, fields) ->
    Hashtbl.remove types struct_name;
    let fixed_fields =
      List.map
        (fun (name, ctype) ->
           if String.ends_with ~suffix:".t" ctype
           then (
             let type_name = String.sub ctype 0 (String.length ctype - 2) in
             let result =
               match Hashtbl.find types type_name with
               | Enum (_, _) -> "int64_t"
               | Struct (_, _) ->
                 transform_libffi_passable types type_name;
                 type_name
               | _ -> type_name
             in
             name, result)
           else if String.starts_with ~prefix:"array" ctype
           then (
             let with_array_size = String.sub ctype 6 (String.length ctype - 6) in
             let index = String.index with_array_size ' ' in
             let element_type =
               String.sub with_array_size index (String.length with_array_size - index)
             in
             name, "ptr" ^ element_type)
           else name, ctype)
        fields
    in
    Hashtbl.add types struct_name (Struct (is_typedef, fixed_fields))
  | _ -> ()


type function_info = Func of string list * string

let keywords =
  String.split_on_char
    ' '
    "and as assert asr begin class constraint do done downto else end exception external \
     false for fun function functor if in include inherit initializer land lazy let lor \
     lsl lsr lxor match method mod module mutable new nonrec object of open or private \
     rec sig struct then to true try type val virtual when while with"
  |> List.map String.trim


let ( >> ) f g x = x |> f |> g
let is_uppercase_ascii = Char.code >> fun i -> i >= 65 && i <= 90
let parenthesize repr = if String.contains repr ' ' then "(" ^ repr ^ ")" else repr

let strip_prefix ~prefix name =
  if String.starts_with ~prefix name
  then (
    let index = String.index name ' ' in
    String.sub name (index + 1) (String.length name - index - 1), false)
  else name, true


let rec find p s i =
  if i = String.length s
  then None
  else if String.get s i |> p
  then Some i
  else find p s (i + 1)


let rec split predicate string =
  if String.equal string String.empty
  then []
  else (
    match find predicate string 0 with
    | Some 0 ->
      let first = String.get string 0 |> Char.escaped in
      let result = split predicate (String.sub string 1 (String.length string - 1)) in
      if List.is_empty result
      then [ string ]
      else (first ^ List.hd result) :: List.tl result
    | Some i ->
      String.sub string 0 i
      :: split predicate (String.sub string i (String.length string - i))
    | None -> [ string ])


let to_snake_case =
  split is_uppercase_ascii
  >> List.map
       (String.lowercase_ascii
        >> fun elem ->
        if String.ends_with ~suffix:"_" elem
        then String.sub elem 0 (String.length elem - 1)
        else elem)
  >> String.concat "_"


let to_ocaml_name string =
  if String.starts_with ~prefix:"_" string
  then "Priv" ^ string
  else if List.exists (( = ) string) keywords
  then string ^ "_"
  else string


let to_ml_string name info =
  match info with
  | Enum (is_typedef, cases) ->
    Printf.sprintf
      "module %s = struct\n\
      \  type t =\n\
      \    %s\n\
      \  let mapping = [\n\
      \    %s\n\
      \  ]\n\
      \  let t : t typ = enum \"%s\" ~typedef:%s mapping ~unexpected:(fun _ -> assert \
       false)\n\
       end"
      (to_ocaml_name name)
      (List.map (fun case -> Printf.sprintf "| %s" case) cases |> String.concat "\n    ")
      (List.map
         (fun case -> Printf.sprintf "%s, constant \"%s\" int64_t;" case case)
         cases
       |> String.concat "\n    ")
      name
      (Bool.to_string is_typedef)
  | Struct (is_typedef, fields) ->
    if List.is_empty fields
    then
      Printf.sprintf
        "module %s = struct\n\
        \  type t\n\
        \  let t : t structure typ = %s\n\
        \  let () = seal t\n\
         end"
        (to_ocaml_name name)
        (Printf.sprintf "structure \"%s\"" name
         |> fun ctype ->
         if is_typedef then Printf.sprintf "typedef (%s) \"%s\"" ctype name else ctype)
    else
      Printf.sprintf
        "module %s = struct\n\
        \  type t\n\
        \  let t : t structure typ = %s\n\
        \  %s\n\
        \  let () = seal t\n\
         end"
        (to_ocaml_name name)
        (Printf.sprintf "structure \"%s\"" name
         |> fun ctype ->
         if is_typedef then Printf.sprintf "typedef (%s) \"%s\"" ctype name else ctype)
        (List.map
           (fun (name, ctype) ->
              Printf.sprintf
                "let %s = field t \"%s\" %s"
                (to_snake_case name |> to_ocaml_name)
                name
                (parenthesize ctype))
           fields
         |> String.concat "\n  ")
  | TypeDef (t_sig, c_sig) ->
    Printf.sprintf
      "module %s = struct\n  type t = %s\n  let t : t %styp = typedef (%s) \"%s\"\nend"
      (to_ocaml_name name)
      t_sig
      (if String.starts_with ~prefix:"static_funptr" c_sig then "static_funptr " else "")
      c_sig
      name


let check_const_ctypes ctype repr =
  if CType.is_const_qualified ctype then "const " ^ repr else repr


let check_const_ml ctype repr =
  if CType.is_const_qualified ctype then repr ^ " const" else repr


let rec parse_ml_type cursor types ctype =
  match CType.kind ctype with
  | CXType_Void -> check_const_ml ctype "unit"
  | CXType_Bool -> check_const_ml ctype "bool"
  | CXType_Char_U -> check_const_ml ctype "string"
  | CXType_UChar -> check_const_ml ctype "Unsigned.uchar"
  | CXType_UShort -> check_const_ml ctype "Unsigned.ushort"
  | CXType_UInt -> check_const_ml ctype "Unsigned.uint"
  | CXType_ULong -> check_const_ml ctype "Unsigned.ulong"
  | CXType_ULongLong -> check_const_ml ctype "Unsigned.ullong"
  | CXType_Char_S -> check_const_ml ctype "string"
  | CXType_SChar -> check_const_ml ctype "int"
  | CXType_Short -> check_const_ml ctype "short"
  | CXType_Int ->
    check_const_ml
      ctype
      (let name = Cursor.spelling cursor in
       if String.equal name "bool"
       then (
         print_endline "here";
         "bool")
       else "int")
  | CXType_Long -> check_const_ml ctype "Signed.long"
  | CXType_LongLong -> check_const_ml ctype "Singed.llong"
  | CXType_Float -> check_const_ml ctype "float"
  | CXType_Double -> check_const_ml ctype "double"
  | CXType_FunctionProto ->
    (match CType.get_return_type ctype with
     | Some returnType ->
       let paramTypes = ref [] in
       Cursor.visit_children cursor (fun cursor _ ->
         match Cursor.kind cursor with
         | CXCursor_ParmDecl ->
           let paramType = parse_ml_type cursor types (Cursor.ctype cursor) in
           paramTypes
           := (if String.ends_with ~suffix:"structure" paramType
               then paramType ^ " ptr"
               else paramType)
              :: !paramTypes;
           CXChildVisit_Recurse
         | _ -> CXChildVisit_Continue);
       let returnRepr = parse_ml_type cursor types returnType in
       paramTypes
       := (if String.ends_with ~suffix:"structure" returnRepr
           then returnRepr ^ " ptr"
           else returnRepr)
          :: !paramTypes;
       String.concat " -> " (List.rev !paramTypes)
     | None ->
       failwith
         (Printf.sprintf
            "Function prototype %s has no return type!"
            (CType.spelling ctype)))
  | CXType_Unexposed ->
    (match CType.get_canonical_type ctype with
     | Some result -> parse_ml_type cursor types result
     | None ->
       failwith
         (Printf.sprintf
            "Unexposed type %s does not have a canonical type!"
            (CType.spelling ctype)))
  | CXType_Pointer ->
    (match CType.get_pointee_type ctype with
     | Some pointeeType ->
       let pointeeKind = CType.kind pointeeType in
       let repr = parse_ml_type cursor types pointeeType in
       if
         pointeeKind != CXType_Unexposed
         && pointeeKind != CXType_FunctionProto
         && pointeeKind != CXType_Char_S
         && pointeeKind != CXType_Char_U
       then repr ^ " ptr"
       else repr
     | None ->
       failwith (Printf.sprintf "Pointer %s has no pointee type!" (CType.spelling ctype)))
  | _ ->
    let without_const, _ = CType.spelling ctype |> strip_prefix ~prefix:"const " in
    let without_enum, _ = without_const |> strip_prefix ~prefix:"enum " in
    let name, is_typedef = without_enum |> strip_prefix ~prefix:"struct " in
    let exists, is_struct =
      match Hashtbl.find_opt types name with
      | Some (Struct (_, _)) -> true, true
      | Some _ -> true, false
      | None -> false, false
    in
    if String.equal name "size_t"
    then "Unsigned.size_t"
    else if String.equal name "intptr_t"
    then "Intptr.t"
    else if String.equal name "int8_t" || String.equal name "int16_t"
    then "int"
    else if String.equal name "int32_t"
    then "int32"
    else if String.equal name "int64_t"
    then "int64"
    else if String.equal name "uint8_t"
    then "Unsigned.uint8"
    else if String.equal name "uint16_t"
    then "Unsigned.uint16"
    else if String.equal name "uint32_t"
    then "Unsigned.uint32"
    else if String.equal name "uint64_t"
    then "Unsigned.uint64"
    else if not exists
    then "unit"
    else
      to_ocaml_name name
      ^ ".t"
      ^ if (not is_typedef) || is_struct then " structure" else ""


let rec parse_c_type
          (types, order)
          ~libffi_passable
          ~struct_compatible
          cursor
          parent
          ctype
  =
  match CType.kind ctype with
  | CXType_Void -> check_const_ctypes ctype "void"
  | CXType_Bool -> check_const_ctypes ctype "bool"
  | CXType_Char_U -> "string"
  | CXType_UChar -> check_const_ctypes ctype "uchar"
  | CXType_UShort -> check_const_ctypes ctype "ushort"
  | CXType_UInt -> check_const_ctypes ctype "uint"
  | CXType_ULong -> check_const_ctypes ctype "ulong"
  | CXType_ULongLong -> check_const_ctypes ctype "ullong"
  | CXType_Char_S -> "string"
  | CXType_SChar -> check_const_ctypes ctype "int"
  | CXType_Short -> check_const_ctypes ctype "short"
  | CXType_Int ->
    check_const_ctypes
      ctype
      (let name = CType.spelling ctype in
       if String.equal name "bool" then "bool" else "int")
  | CXType_Long -> check_const_ctypes ctype "long"
  | CXType_LongLong -> check_const_ctypes ctype "llong"
  | CXType_Float -> check_const_ctypes ctype "float"
  | CXType_Double -> check_const_ctypes ctype "double"
  | CXType_FunctionProto ->
    (match CType.get_return_type ctype with
     | Some result ->
       let returnType =
         parse_c_type
           (types, order)
           ~libffi_passable:true
           ~struct_compatible
           cursor
           parent
           result
       in
       let paramTypes = ref [] in
       Cursor.visit_children cursor (fun cursor _ ->
         match Cursor.kind cursor with
         | CXCursor_ParmDecl ->
           let paramType =
             parse_c_type
               (types, order)
               ~libffi_passable:true
               ~struct_compatible
               cursor
               parent
               (Cursor.ctype cursor)
           in
           paramTypes
           := (if
                 struct_compatible
                 && (not (String.starts_with ~prefix:"ptr" paramType))
                 && String.ends_with ~suffix:".t" paramType
               then (
                 match
                   Hashtbl.find
                     types
                     (String.sub paramType 0 (String.length paramType - 2))
                 with
                 | Struct (_, _) -> "ptr (const " ^ parenthesize paramType ^ ")"
                 | _ -> paramType)
               else paramType)
              :: !paramTypes;
           CXChildVisit_Recurse
         | _ -> CXChildVisit_Continue);
       Printf.sprintf
         "static_funptr (%s @-> returning %s)"
         (String.concat " @-> " (List.rev !paramTypes))
         ((if
             (not (String.starts_with ~prefix:"ptr" returnType))
             && String.ends_with ~suffix:".t" returnType
           then (
             match
               Hashtbl.find types (String.sub returnType 0 (String.length returnType - 2))
             with
             | Struct (_, _) -> "ptr (const " ^ parenthesize returnType ^ ")"
             | _ -> returnType)
           else returnType)
          |> parenthesize)
     | None ->
       failwith
         (Printf.sprintf
            "Function prototype %s has no return type!"
            (CType.spelling ctype)))
  | CXType_Unexposed ->
    (match CType.get_canonical_type ctype with
     | Some result ->
       parse_c_type
         (types, order)
         ~libffi_passable
         ~struct_compatible
         cursor
         parent
         result
     | None ->
       failwith
         (Printf.sprintf
            "Unexposed type %s does not have a canonical type!"
            (CType.spelling ctype)))
  | CXType_Pointer ->
    (match CType.get_pointee_type ctype with
     | Some pointeeType ->
       let without_const, is_not_const =
         CType.spelling pointeeType |> strip_prefix ~prefix:"const "
       in
       let without_enum, _ = without_const |> strip_prefix ~prefix:"enum " in
       let pointeeName, _ = without_enum |> strip_prefix ~prefix:"struct " in
       let repr =
         parse_c_type
           (types, order)
           ~libffi_passable:false
           ~struct_compatible
           cursor
           parent
           pointeeType
       in
       if struct_compatible && Hashtbl.mem types pointeeName
       then (
         match Hashtbl.find types pointeeName with
         | Struct (_, fields) ->
           if List.is_empty fields
           then (
             Hashtbl.remove types pointeeName;
             order := List.filter (String.equal pointeeName >> not) !order;
             if is_not_const then "ptr void" else "ptr (const void)")
           else "ptr " ^ parenthesize repr
         | _ -> "ptr " ^ parenthesize repr)
       else (
         let pointeeKind = CType.kind pointeeType in
         if
           pointeeKind != CXType_Unexposed
           && pointeeKind != CXType_FunctionProto
           && pointeeKind != CXType_Char_S
           && pointeeKind != CXType_Char_U
         then "ptr " ^ parenthesize repr
         else repr)
     | None ->
       failwith (Printf.sprintf "Pointer %s has no pointee type!" (CType.spelling ctype)))
  | CXType_ConstantArray ->
    (match CType.get_constant_array_data ctype with
     | Some (element_type, size) ->
       Printf.sprintf
         "array %s %s"
         (Int64.to_string size)
         (parse_c_type
            (types, order)
            ~libffi_passable:false
            ~struct_compatible
            cursor
            parent
            element_type
          |> parenthesize)
     | None ->
       failwith
         (Printf.sprintf
            "Constant array %s has no element type nor size!"
            (CType.spelling ctype)))
  | _ ->
    let initial = CType.spelling ctype in
    let without_const, is_not_const = initial |> strip_prefix ~prefix:"const " in
    let without_enum, _ = without_const |> strip_prefix ~prefix:"enum " in
    let name, _ = without_enum |> strip_prefix ~prefix:"struct " in
    if
      String.equal name "size_t"
      || String.equal name "intptr_t"
      || String.equal name "int8_t"
      || String.equal name "int16_t"
      || String.equal name "int32_t"
      || String.equal name "int64_t"
      || String.equal name "uint8_t"
      || String.equal name "uint16_t"
      || String.equal name "uint32_t"
      || String.equal name "uint64_t"
    then initial
    else (
      if not (Hashtbl.mem types name)
      then (
        Cursor.visit_children
          parent
          (collect_types (types, order) SourceLocation.is_in_system_header (( = ) name));
        assert (Hashtbl.mem types name))
      else ();
      (match Hashtbl.find types name with
       | Struct (_, _) ->
         if libffi_passable then transform_libffi_passable types name else ()
       | _ -> ());
      if is_not_const
      then to_ocaml_name name ^ ".t"
      else "const " ^ to_ocaml_name name ^ ".t")


and collect_types (types, order) location_predicate name_predicate cursor parent =
  if not (location_predicate (Cursor.location cursor))
  then CXChildVisit_Continue
  else (
    match Cursor.kind cursor with
    | CXCursor_TypedefDecl ->
      let ctype = Cursor.ctype cursor in
      (match CType.get_canonical_type ctype with
       | Some canonical_type ->
         (match CType.kind canonical_type with
          | CXType_Record ->
            let name, is_typedef =
              CType.spelling canonical_type |> strip_prefix ~prefix:"struct "
            in
            if (not is_typedef) && Hashtbl.mem types name
            then (
              match Hashtbl.find types name with
              | Struct (_, fields) ->
                Hashtbl.remove types name;
                Hashtbl.add types name (Struct (true, fields))
              | _ -> failwith (Printf.sprintf "Invalid type for %s" name))
            else ()
          | CXType_Enum ->
            let name, is_typedef =
              CType.spelling canonical_type |> strip_prefix ~prefix:"enum "
            in
            if (not is_typedef) && Hashtbl.mem types name
            then (
              match Hashtbl.find types name with
              | Enum (_, cases) ->
                Hashtbl.remove types name;
                Hashtbl.add types name (Enum (true, cases))
              | _ -> failwith (Printf.sprintf "Invalid type for %s" name))
            else ()
          | _ ->
            let name = CType.spelling ctype in
            if Hashtbl.mem types name || not (name_predicate name)
            then ()
            else (
              let c_sig =
                parse_c_type
                  (types, order)
                  ~libffi_passable:false
                  ~struct_compatible:true
                  cursor
                  parent
                  canonical_type
              in
              let ml_sig = parse_ml_type cursor types canonical_type in
              Hashtbl.add types name (TypeDef (ml_sig, c_sig));
              order := name :: !order))
       | None ->
         failwith
           (Printf.sprintf
              "Typedef %s does not provide a canonical type!"
              (CType.spelling ctype)));
      CXChildVisit_Continue
    | CXCursor_EnumDecl ->
      let name, is_typedef =
        Cursor.ctype cursor |> CType.spelling |> strip_prefix ~prefix:"enum "
      in
      if Hashtbl.mem types name || not (name_predicate name)
      then CXChildVisit_Continue
      else (
        let cases = ref [] in
        Cursor.visit_children cursor (fun cursor _ ->
          match Cursor.kind cursor with
          | CXCursor_EnumConstantDecl ->
            cases := Cursor.spelling cursor :: !cases;
            CXChildVisit_Recurse
          | _ -> CXChildVisit_Continue);
        Hashtbl.add types name (Enum (is_typedef, List.rev !cases));
        order := name :: !order;
        CXChildVisit_Recurse)
    | CXCursor_StructDecl ->
      let name, is_typedef =
        Cursor.ctype cursor |> CType.spelling |> strip_prefix ~prefix:"struct "
      in
      if Hashtbl.mem types name || not (name_predicate name)
      then CXChildVisit_Continue
      else (
        let fields = ref [] in
        Cursor.visit_children cursor (fun cursor _ ->
          match Cursor.kind cursor with
          | CXCursor_FieldDecl ->
            let fieldName = Cursor.spelling cursor in
            if String.equal fieldName String.empty || Cursor.is_bit_field cursor
            then CXChildVisit_Continue
            else (
              let fieldType =
                Cursor.ctype cursor
                |> parse_c_type
                     (types, order)
                     ~libffi_passable:false
                     ~struct_compatible:true
                     cursor
                     parent
              in
              fields := (fieldName, fieldType) :: !fields;
              CXChildVisit_Recurse)
          | _ -> CXChildVisit_Continue);
        Hashtbl.add types name (Struct (is_typedef, List.rev !fields));
        order := name :: !order;
        CXChildVisit_Recurse)
    | _ -> CXChildVisit_Recurse)


let collect_functions types functions cursor parent =
  if not (SourceLocation.is_from_main_file (Cursor.location cursor))
  then CXChildVisit_Continue
  else (
    match Cursor.kind cursor with
    | CXCursor_FunctionDecl ->
      let ctype = Cursor.ctype cursor in
      let name = Cursor.spelling cursor in
      if Hashtbl.mem functions name
      then CXChildVisit_Continue
      else (
        match CType.get_return_type ctype with
        | Some result ->
          let returnType =
            parse_c_type
              types
              ~libffi_passable:false
              ~struct_compatible:false
              cursor
              parent
              result
          in
          let paramTypes = ref [] in
          Cursor.visit_children cursor (fun cursor _ ->
            match Cursor.kind cursor with
            | CXCursor_ParmDecl ->
              let paramType =
                parse_c_type
                  types
                  ~libffi_passable:false
                  ~struct_compatible:false
                  cursor
                  parent
                  (Cursor.ctype cursor)
              in
              paramTypes
              := (if String.starts_with ~prefix:"static_funptr" paramType
                  then (
                    let index = String.index paramType '(' in
                    "static_funptr Ctypes."
                    ^ String.sub paramType index (String.length paramType - index))
                  else paramType)
                 :: !paramTypes;
              CXChildVisit_Continue
            | _ -> CXChildVisit_Continue);
          Hashtbl.add functions name (Func (List.rev !paramTypes, returnType));
          CXChildVisit_Recurse
        | None ->
          failwith
            (Printf.sprintf
               "Function prototype %s has no return type!"
               (CType.spelling ctype)))
    | _ -> CXChildVisit_Recurse)


let traverse_file filename f =
  with_index
    0
    0
    (with_translation_unit filename [ "-I/usr/lib/clang/19/include" ] (with_cursor f))


let collect_bindings types functions file =
  traverse_file file (fun cursor ->
    Cursor.visit_children
      cursor
      (collect_types types SourceLocation.is_from_main_file (fun _ -> true));
    Cursor.visit_children cursor (collect_functions types functions))


let indent indent =
  String.split_on_char '\n'
  >> List.map (( ^ ) (String.make indent ' '))
  >> String.concat "\n"


let write_types types order =
  let out = open_out "type_bindings.ml" in
  let fmt = Format.formatter_of_out_channel out in
  Format.fprintf fmt "open Ctypes\n";
  Format.fprintf fmt "\n";
  Format.fprintf fmt "module Types (T : TYPE) = struct\n";
  Format.fprintf fmt "  open T\n";
  Format.fprintf fmt "\n";
  List.iter
    (fun name ->
       let info = Hashtbl.find types name in
       Format.fprintf fmt "%s\n\n" (info |> to_ml_string name |> indent 2))
    (List.rev !order);
  Format.fprintf fmt "end";
  Format.pp_print_flush fmt ();
  close_out out


let write_functions functions =
  let out = open_out "function_bindings.ml" in
  let fmt = Format.formatter_of_out_channel out in
  Format.fprintf fmt "open Ctypes\n";
  Format.fprintf fmt "open Types_generated\n";
  Format.fprintf fmt "\n";
  Format.fprintf fmt "module Functions (F : FOREIGN) = struct\n";
  Format.fprintf fmt "  open F\n";
  Format.fprintf fmt "\n";
  Hashtbl.iter
    (fun name (Func (params, return)) ->
       Format.fprintf
         fmt
         "  let %s = foreign \"%s\" (%s @-> returning %s)\n\n"
         (to_snake_case name)
         name
         (if List.is_empty params then "void" else String.concat " @-> " params)
         (parenthesize return))
    functions;
  Format.fprintf fmt "end";
  Format.pp_print_flush fmt ();
  close_out out


let () =
  let exclude_prefix, files =
    if String.equal "-exclude-header-prefix" Sys.argv.(1)
    then Sys.argv.(2), Array.sub Sys.argv 3 (Array.length Sys.argv - 3)
    else String.empty, Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
  in
  let temp_files =
    List.map
      (fun file ->
         let temp_file = "temp_" ^ Filename.basename file in
         let process =
           Printf.sprintf
             "clang -Xclang -fkeep-system-includes %s-E %s > %s"
             (if String.equal String.empty exclude_prefix
              then String.empty
              else "--no-system-header-prefix=" ^ exclude_prefix ^ " ")
             file
             temp_file
           |> Unix.open_process_in
         in
         let _ = In_channel.input_all process in
         In_channel.close process;
         temp_file)
      (Array.to_list files)
  in
  let types = Hashtbl.create 10 in
  let type_order = ref [] in
  let functions = Hashtbl.create 10 in
  List.iter
    (fun file ->
       collect_bindings (types, type_order) functions file;
       Sys.remove file)
    temp_files;
  write_types types type_order;
  write_functions functions

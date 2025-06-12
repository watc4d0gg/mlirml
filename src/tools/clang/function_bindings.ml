open Ctypes
open Types_generated

module Functions (F : FOREIGN) = struct
  open F

  let clang_get_completion_brief_comment =
    foreign
      "clang_getCompletionBriefComment"
      (CXCompletionString.t @-> returning CXString.t)


  let clang_cursor_get_spelling_name_range =
    foreign
      "clang_Cursor_getSpellingNameRange"
      (CXCursor.t @-> uint @-> uint @-> returning CXSourceRange.t)


  let clang_eval_result_get_kind =
    foreign "clang_EvalResult_getKind" (CXEvalResult.t @-> returning CXEvalResultKind.t)


  let clang_dispose_cxplatform_availability =
    foreign
      "clang_disposeCXPlatformAvailability"
      (ptr CXPlatformAvailability.t @-> returning void)


  let clang_get_skipped_ranges =
    foreign
      "clang_getSkippedRanges"
      (CXTranslationUnit.t @-> CXFile.t @-> returning (ptr CXSourceRangeList.t))


  let clang_cxxconstructor_is_copy_constructor =
    foreign "clang_CXXConstructor_isCopyConstructor" (CXCursor.t @-> returning uint)


  let clang_cursor_get_template_argument_value =
    foreign
      "clang_Cursor_getTemplateArgumentValue"
      (CXCursor.t @-> uint @-> returning llong)


  let clang_get_file =
    foreign "clang_getFile" (CXTranslationUnit.t @-> string @-> returning CXFile.t)


  let clang_is_function_type_variadic =
    foreign "clang_isFunctionTypeVariadic" (CXType.t @-> returning uint)


  let clang_get_cursor_lexical_parent =
    foreign "clang_getCursorLexicalParent" (CXCursor.t @-> returning CXCursor.t)


  let clang_get_clang_version =
    foreign "clang_getClangVersion" (void @-> returning CXString.t)


  let clang_get_decl_obj_ctype_encoding =
    foreign "clang_getDeclObjCTypeEncoding" (CXCursor.t @-> returning CXString.t)


  let clang_cxxconstructor_is_move_constructor =
    foreign "clang_CXXConstructor_isMoveConstructor" (CXCursor.t @-> returning uint)


  let clang_module_get_astfile =
    foreign "clang_Module_getASTFile" (CXModule.t @-> returning CXFile.t)


  let clang_cursor_is_variadic =
    foreign "clang_Cursor_isVariadic" (CXCursor.t @-> returning uint)


  let clang_type_get_value_type =
    foreign "clang_Type_getValueType" (CXType.t @-> returning CXType.t)


  let clang_cursor_get_template_argument_unsigned_value =
    foreign
      "clang_Cursor_getTemplateArgumentUnsignedValue"
      (CXCursor.t @-> uint @-> returning ullong)


  let clang_get_cursor_exception_specification_type =
    foreign "clang_getCursorExceptionSpecificationType" (CXCursor.t @-> returning int)


  let clang_get_type_kind_spelling =
    foreign "clang_getTypeKindSpelling" (CXTypeKind.t @-> returning CXString.t)


  let clang_is_expression =
    foreign "clang_isExpression" (CXCursorKind.t @-> returning uint)


  let clang_create_translation_unit =
    foreign
      "clang_createTranslationUnit"
      (CXIndex.t @-> string @-> returning CXTranslationUnit.t)


  let clang_get_all_skipped_ranges =
    foreign
      "clang_getAllSkippedRanges"
      (CXTranslationUnit.t @-> returning (ptr CXSourceRangeList.t))


  let clang_get_cursor_binary_operator_kind =
    foreign
      "clang_getCursorBinaryOperatorKind"
      (CXCursor.t @-> returning CXBinaryOperatorKind.t)


  let clang_cxxmethod_is_pure_virtual =
    foreign "clang_CXXMethod_isPureVirtual" (CXCursor.t @-> returning uint)


  let clang_type_get_cxxref_qualifier =
    foreign "clang_Type_getCXXRefQualifier" (CXType.t @-> returning CXRefQualifierKind.t)


  let clang_get_translation_unit_target_info =
    foreign
      "clang_getTranslationUnitTargetInfo"
      (CXTranslationUnit.t @-> returning CXTargetInfo.t)


  let clang_get_diagnostic =
    foreign
      "clang_getDiagnostic"
      (CXTranslationUnit.t @-> uint @-> returning CXDiagnostic.t)


  let clang_construct_usr_obj_cproperty =
    foreign
      "clang_constructUSR_ObjCProperty"
      (string @-> CXString.t @-> returning CXString.t)


  let clang_type_get_named_type =
    foreign "clang_Type_getNamedType" (CXType.t @-> returning CXType.t)


  let clang_cursor_is_null = foreign "clang_Cursor_isNull" (CXCursor.t @-> returning int)

  let clang_remap_dispose =
    foreign "clang_remap_dispose" (CXRemapping.t @-> returning void)


  let clang_remap_get_filenames =
    foreign
      "clang_remap_getFilenames"
      (CXRemapping.t @-> uint @-> ptr CXString.t @-> ptr CXString.t @-> returning void)


  let clang_cursor_get_binary_opcode =
    foreign
      "clang_Cursor_getBinaryOpcode"
      (CXCursor.t @-> returning CX_BinaryOperatorKind.t)


  let clang_type_visit_fields =
    foreign
      "clang_Type_visitFields"
      (CXType.t @-> CXFieldVisitor.t @-> CXClientData.t @-> returning uint)


  let clang_get_null_range =
    foreign "clang_getNullRange" (void @-> returning CXSourceRange.t)


  let clang_cursor_get_translation_unit =
    foreign
      "clang_Cursor_getTranslationUnit"
      (CXCursor.t @-> returning CXTranslationUnit.t)


  let clang_is_declaration =
    foreign "clang_isDeclaration" (CXCursorKind.t @-> returning uint)


  let clang_is_file_multiple_include_guarded =
    foreign
      "clang_isFileMultipleIncludeGuarded"
      (CXTranslationUnit.t @-> CXFile.t @-> returning uint)


  let clang_printing_policy_get_property =
    foreign
      "clang_PrintingPolicy_getProperty"
      (CXPrintingPolicy.t @-> CXPrintingPolicyProperty.t @-> returning uint)


  let clang_is_virtual_base = foreign "clang_isVirtualBase" (CXCursor.t @-> returning uint)

  let clang_get_completion_annotation =
    foreign
      "clang_getCompletionAnnotation"
      (CXCompletionString.t @-> uint @-> returning CXString.t)


  let clang_index_loc_get_file_location =
    foreign
      "clang_indexLoc_getFileLocation"
      (CXIdxLoc.t
       @-> ptr CXIdxClientFile.t
       @-> ptr CXFile.t
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_cxxrecord_is_abstract =
    foreign "clang_CXXRecord_isAbstract" (CXCursor.t @-> returning uint)


  let clang_module_get_top_level_header =
    foreign
      "clang_Module_getTopLevelHeader"
      (CXTranslationUnit.t @-> CXModule.t @-> uint @-> returning CXFile.t)


  let clang_hash_cursor = foreign "clang_hashCursor" (CXCursor.t @-> returning uint)

  let clang_get_null_location =
    foreign "clang_getNullLocation" (void @-> returning CXSourceLocation.t)


  let clang_module_is_system =
    foreign "clang_Module_isSystem" (CXModule.t @-> returning int)


  let clang_get_cstring = foreign "clang_getCString" (CXString.t @-> returning string)

  let clang_get_completion_availability =
    foreign
      "clang_getCompletionAvailability"
      (CXCompletionString.t @-> returning CXAvailabilityKind.t)


  let clang_type_get_template_argument_as_type =
    foreign
      "clang_Type_getTemplateArgumentAsType"
      (CXType.t @-> uint @-> returning CXType.t)


  let clang_get_address_space =
    foreign "clang_getAddressSpace" (CXType.t @-> returning uint)


  let clang_is_attribute = foreign "clang_isAttribute" (CXCursorKind.t @-> returning uint)

  let clang_construct_usr_obj_civar =
    foreign "clang_constructUSR_ObjCIvar" (string @-> CXString.t @-> returning CXString.t)


  let clang_get_completion_num_annotations =
    foreign "clang_getCompletionNumAnnotations" (CXCompletionString.t @-> returning uint)


  let clang_get_cursor_usr =
    foreign "clang_getCursorUSR" (CXCursor.t @-> returning CXString.t)


  let clang_get_cursor_language =
    foreign "clang_getCursorLanguage" (CXCursor.t @-> returning CXLanguageKind.t)


  let clang_sort_code_completion_results =
    foreign
      "clang_sortCodeCompletionResults"
      (ptr CXCompletionResult.t @-> uint @-> returning void)


  let clang_get_cursor_printing_policy =
    foreign "clang_getCursorPrintingPolicy" (CXCursor.t @-> returning CXPrintingPolicy.t)


  let clang_cursor_is_macro_function_like =
    foreign "clang_Cursor_isMacroFunctionLike" (CXCursor.t @-> returning uint)


  let clang_get_cursor_linkage =
    foreign "clang_getCursorLinkage" (CXCursor.t @-> returning CXLinkageKind.t)


  let clang_construct_usr_obj_ccategory =
    foreign "clang_constructUSR_ObjCCategory" (string @-> string @-> returning CXString.t)


  let clang_cxxconstructor_is_default_constructor =
    foreign "clang_CXXConstructor_isDefaultConstructor" (CXCursor.t @-> returning uint)


  let clang_is_restrict_qualified_type =
    foreign "clang_isRestrictQualifiedType" (CXType.t @-> returning uint)


  let clang_cursor_get_argument =
    foreign "clang_Cursor_getArgument" (CXCursor.t @-> uint @-> returning CXCursor.t)


  let clang_target_info_dispose =
    foreign "clang_TargetInfo_dispose" (CXTargetInfo.t @-> returning void)


  let clang_get_diagnostic_set_from_tu =
    foreign
      "clang_getDiagnosticSetFromTU"
      (CXTranslationUnit.t @-> returning CXDiagnosticSet.t)


  let clang_get_array_element_type =
    foreign "clang_getArrayElementType" (CXType.t @-> returning CXType.t)


  let clang_dispose_translation_unit =
    foreign "clang_disposeTranslationUnit" (CXTranslationUnit.t @-> returning void)


  let clang_get_cursor_availability =
    foreign "clang_getCursorAvailability" (CXCursor.t @-> returning CXAvailabilityKind.t)


  let clang_get_instantiation_location =
    foreign
      "clang_getInstantiationLocation"
      (CXSourceLocation.t
       @-> ptr CXFile.t
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_get_file_contents =
    foreign
      "clang_getFileContents"
      (CXTranslationUnit.t @-> CXFile.t @-> ptr size_t @-> returning string)


  let clang_find_references_in_file_with_block =
    foreign
      "clang_findReferencesInFileWithBlock"
      (CXCursor.t @-> CXFile.t @-> CXCursorAndRangeVisitorBlock.t @-> returning CXResult.t)


  let clang_find_references_in_file =
    foreign
      "clang_findReferencesInFile"
      (CXCursor.t @-> CXFile.t @-> CXCursorAndRangeVisitor.t @-> returning CXResult.t)


  let clang_get_completion_chunk_text =
    foreign
      "clang_getCompletionChunkText"
      (CXCompletionString.t @-> uint @-> returning CXString.t)


  let clang_get_completion_priority =
    foreign "clang_getCompletionPriority" (CXCompletionString.t @-> returning uint)


  let clang_cxxfield_is_mutable =
    foreign "clang_CXXField_isMutable" (CXCursor.t @-> returning uint)


  let clang_type_get_obj_cencoding =
    foreign "clang_Type_getObjCEncoding" (CXType.t @-> returning CXString.t)


  let clang_cursor_has_attrs =
    foreign "clang_Cursor_hasAttrs" (CXCursor.t @-> returning uint)


  let clang_eval_result_get_as_long_long =
    foreign "clang_EvalResult_getAsLongLong" (CXEvalResult.t @-> returning llong)


  let clang_find_includes_in_file_with_block =
    foreign
      "clang_findIncludesInFileWithBlock"
      (CXTranslationUnit.t
       @-> CXFile.t
       @-> CXCursorAndRangeVisitorBlock.t
       @-> returning CXResult.t)


  let clang_cursor_is_anonymous =
    foreign "clang_Cursor_isAnonymous" (CXCursor.t @-> returning uint)


  let clang_index_get_client_entity =
    foreign
      "clang_index_getClientEntity"
      (ptr (const CXIdxEntityInfo.t) @-> returning CXIdxClientEntity.t)


  let clang_cxcursor_set_insert =
    foreign "clang_CXCursorSet_insert" (CXCursorSet.t @-> CXCursor.t @-> returning uint)


  let clang_type_get_modified_type =
    foreign "clang_Type_getModifiedType" (CXType.t @-> returning CXType.t)


  let clang_type_get_num_obj_cprotocol_refs =
    foreign "clang_Type_getNumObjCProtocolRefs" (CXType.t @-> returning uint)


  let clang_find_includes_in_file =
    foreign
      "clang_findIncludesInFile"
      (CXTranslationUnit.t
       @-> CXFile.t
       @-> CXCursorAndRangeVisitor.t
       @-> returning CXResult.t)


  let clang_dispose_tokens =
    foreign
      "clang_disposeTokens"
      (CXTranslationUnit.t @-> ptr CXToken.t @-> uint @-> returning void)


  let clang_get_type_spelling =
    foreign "clang_getTypeSpelling" (CXType.t @-> returning CXString.t)


  let clang_cursor_is_dynamic_call =
    foreign "clang_Cursor_isDynamicCall" (CXCursor.t @-> returning int)


  let clang_get_canonical_type =
    foreign "clang_getCanonicalType" (CXType.t @-> returning CXType.t)


  let clang_cxindex_get_global_options =
    foreign "clang_CXIndex_getGlobalOptions" (CXIndex.t @-> returning uint)


  let clang_dispose_cxcursor_set =
    foreign "clang_disposeCXCursorSet" (CXCursorSet.t @-> returning void)


  let clang_get_binary_operator_kind_spelling =
    foreign
      "clang_getBinaryOperatorKindSpelling"
      (CXBinaryOperatorKind.t @-> returning CXString.t)


  let clang_cursor_get_offset_of_field =
    foreign "clang_Cursor_getOffsetOfField" (CXCursor.t @-> returning llong)


  let clang_get_turesource_usage_name =
    foreign "clang_getTUResourceUsageName" (CXTUResourceUsageKind.t @-> returning string)


  let clang_type_get_nullability =
    foreign "clang_Type_getNullability" (CXType.t @-> returning CXTypeNullabilityKind.t)


  let clang_cursor_get_num_template_arguments =
    foreign "clang_Cursor_getNumTemplateArguments" (CXCursor.t @-> returning int)


  let clang_get_non_reference_type =
    foreign "clang_getNonReferenceType" (CXType.t @-> returning CXType.t)


  let clang_get_cursor_type =
    foreign "clang_getCursorType" (CXCursor.t @-> returning CXType.t)


  let clang_type_get_num_obj_ctype_args =
    foreign "clang_Type_getNumObjCTypeArgs" (CXType.t @-> returning uint)


  let clang_get_spelling_location =
    foreign
      "clang_getSpellingLocation"
      (CXSourceLocation.t
       @-> ptr CXFile.t
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_cxxmethod_is_deleted =
    foreign "clang_CXXMethod_isDeleted" (CXCursor.t @-> returning uint)


  let clang_get_cursor_definition =
    foreign "clang_getCursorDefinition" (CXCursor.t @-> returning CXCursor.t)


  let clang_type_get_num_template_arguments =
    foreign "clang_Type_getNumTemplateArguments" (CXType.t @-> returning int)


  let clang_get_presumed_location =
    foreign
      "clang_getPresumedLocation"
      (CXSourceLocation.t @-> ptr CXString.t @-> ptr uint @-> ptr uint @-> returning void)


  let clang_create_index_with_options =
    foreign
      "clang_createIndexWithOptions"
      (ptr (const CXIndexOptions.t) @-> returning CXIndex.t)


  let clang_printing_policy_dispose =
    foreign "clang_PrintingPolicy_dispose" (CXPrintingPolicy.t @-> returning void)


  let clang_location_is_from_main_file =
    foreign "clang_Location_isFromMainFile" (CXSourceLocation.t @-> returning int)


  let clang_parse_translation_unit2 =
    foreign
      "clang_parseTranslationUnit2"
      (CXIndex.t
       @-> string
       @-> ptr string
       @-> int
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> uint
       @-> ptr CXTranslationUnit.t
       @-> returning CXErrorCode.t)


  let clang_dispose_string_set =
    foreign "clang_disposeStringSet" (ptr CXStringSet.t @-> returning void)


  let clang_index_get_obj_cinterface_decl_info =
    foreign
      "clang_index_getObjCInterfaceDeclInfo"
      (ptr (const CXIdxDeclInfo.t)
       @-> returning (ptr (const CXIdxObjCInterfaceDeclInfo.t)))


  let clang_get_arg_type =
    foreign "clang_getArgType" (CXType.t @-> uint @-> returning CXType.t)


  let clang_create_index =
    foreign "clang_createIndex" (int @-> int @-> returning CXIndex.t)


  let clang_type_is_transparent_tag_typedef =
    foreign "clang_Type_isTransparentTagTypedef" (CXType.t @-> returning uint)


  let clang_cursor_get_obj_cmanglings =
    foreign "clang_Cursor_getObjCManglings" (CXCursor.t @-> returning (ptr CXStringSet.t))


  let clang_default_reparse_options =
    foreign "clang_defaultReparseOptions" (CXTranslationUnit.t @-> returning uint)


  let clang_get_file_location =
    foreign
      "clang_getFileLocation"
      (CXSourceLocation.t
       @-> ptr CXFile.t
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_is_volatile_qualified_type =
    foreign "clang_isVolatileQualifiedType" (CXType.t @-> returning uint)


  let clang_create_translation_unit_from_source_file =
    foreign
      "clang_createTranslationUnitFromSourceFile"
      (CXIndex.t
       @-> string
       @-> int
       @-> ptr string
       @-> uint
       @-> ptr CXUnsavedFile.t
       @-> returning CXTranslationUnit.t)


  let clang_get_cursor_display_name =
    foreign "clang_getCursorDisplayName" (CXCursor.t @-> returning CXString.t)


  let clang_is_const_qualified_type =
    foreign "clang_isConstQualifiedType" (CXType.t @-> returning uint)


  let clang_get_pointee_type =
    foreign "clang_getPointeeType" (CXType.t @-> returning CXType.t)


  let clang_is_preprocessing =
    foreign "clang_isPreprocessing" (CXCursorKind.t @-> returning uint)


  let clang_is_translation_unit =
    foreign "clang_isTranslationUnit" (CXCursorKind.t @-> returning uint)


  let clang_is_statement = foreign "clang_isStatement" (CXCursorKind.t @-> returning uint)

  let clang_cxxmethod_is_copy_assignment_operator =
    foreign "clang_CXXMethod_isCopyAssignmentOperator" (CXCursor.t @-> returning uint)


  let clang_cursor_get_mangling =
    foreign "clang_Cursor_getMangling" (CXCursor.t @-> returning CXString.t)


  let clang_index_get_iboutlet_collection_attr_info =
    foreign
      "clang_index_getIBOutletCollectionAttrInfo"
      (ptr (const CXIdxAttrInfo.t)
       @-> returning (ptr (const CXIdxIBOutletCollectionAttrInfo.t)))


  let clang_get_module_for_file =
    foreign
      "clang_getModuleForFile"
      (CXTranslationUnit.t @-> CXFile.t @-> returning CXModule.t)


  let clang_equal_locations =
    foreign
      "clang_equalLocations"
      (CXSourceLocation.t @-> CXSourceLocation.t @-> returning uint)


  let clang_index_get_obj_cprotocol_ref_list_info =
    foreign
      "clang_index_getObjCProtocolRefListInfo"
      (ptr (const CXIdxDeclInfo.t)
       @-> returning (ptr (const CXIdxObjCProtocolRefListInfo.t)))


  let clang_index_action_create =
    foreign "clang_IndexAction_create" (CXIndex.t @-> returning CXIndexAction.t)


  let clang_index_set_client_entity =
    foreign
      "clang_index_setClientEntity"
      (ptr (const CXIdxEntityInfo.t) @-> CXIdxClientEntity.t @-> returning void)


  let clang_enum_decl_is_scoped =
    foreign "clang_EnumDecl_isScoped" (CXCursor.t @-> returning uint)


  let clang_is_podtype = foreign "clang_isPODType" (CXType.t @-> returning uint)

  let clang_code_complete_get_num_diagnostics =
    foreign
      "clang_codeCompleteGetNumDiagnostics"
      (ptr CXCodeCompleteResults.t @-> returning uint)


  let clang_get_null_cursor = foreign "clang_getNullCursor" (void @-> returning CXCursor.t)

  let clang_get_range_start =
    foreign "clang_getRangeStart" (CXSourceRange.t @-> returning CXSourceLocation.t)


  let clang_get_included_file =
    foreign "clang_getIncludedFile" (CXCursor.t @-> returning CXFile.t)


  let clang_code_complete_get_contexts =
    foreign
      "clang_codeCompleteGetContexts"
      (ptr CXCodeCompleteResults.t @-> returning ullong)


  let clang_get_num_overloaded_decls =
    foreign "clang_getNumOverloadedDecls" (CXCursor.t @-> returning uint)


  let clang_type_get_size_of =
    foreign "clang_Type_getSizeOf" (CXType.t @-> returning llong)


  let clang_cursor_get_obj_cselector_index =
    foreign "clang_Cursor_getObjCSelectorIndex" (CXCursor.t @-> returning int)


  let clang_type_get_class_type =
    foreign "clang_Type_getClassType" (CXType.t @-> returning CXType.t)


  let clang_cursor_get_obj_cproperty_getter_name =
    foreign "clang_Cursor_getObjCPropertyGetterName" (CXCursor.t @-> returning CXString.t)


  let clang_construct_usr_obj_cprotocol =
    foreign "clang_constructUSR_ObjCProtocol" (string @-> returning CXString.t)


  let clang_cxxmethod_is_virtual =
    foreign "clang_CXXMethod_isVirtual" (CXCursor.t @-> returning uint)


  let clang_cxxmethod_is_static =
    foreign "clang_CXXMethod_isStatic" (CXCursor.t @-> returning uint)


  let clang_type_get_align_of =
    foreign "clang_Type_getAlignOf" (CXType.t @-> returning llong)


  let clang_get_type_declaration =
    foreign "clang_getTypeDeclaration" (CXType.t @-> returning CXCursor.t)


  let clang_get_cursor_visibility =
    foreign "clang_getCursorVisibility" (CXCursor.t @-> returning CXVisibilityKind.t)


  let clang_eval_result_dispose =
    foreign "clang_EvalResult_dispose" (CXEvalResult.t @-> returning void)


  let clang_dispose_overridden_cursors =
    foreign "clang_disposeOverriddenCursors" (ptr CXCursor.t @-> returning void)


  let clang_get_overridden_cursors =
    foreign
      "clang_getOverriddenCursors"
      (CXCursor.t @-> ptr (ptr CXCursor.t) @-> ptr uint @-> returning void)


  let clang_save_translation_unit =
    foreign
      "clang_saveTranslationUnit"
      (CXTranslationUnit.t @-> string @-> uint @-> returning int)


  let clang_type_get_obj_ctype_arg =
    foreign "clang_Type_getObjCTypeArg" (CXType.t @-> uint @-> returning CXType.t)


  let clang_get_typedef_name =
    foreign "clang_getTypedefName" (CXType.t @-> returning CXString.t)


  let clang_cursor_is_macro_builtin =
    foreign "clang_Cursor_isMacroBuiltin" (CXCursor.t @-> returning uint)


  let clang_get_cursor_location =
    foreign "clang_getCursorLocation" (CXCursor.t @-> returning CXSourceLocation.t)


  let clang_get_cursor_kind =
    foreign "clang_getCursorKind" (CXCursor.t @-> returning CXCursorKind.t)


  let clang_is_invalid_declaration =
    foreign "clang_isInvalidDeclaration" (CXCursor.t @-> returning uint)


  let clang_module_get_full_name =
    foreign "clang_Module_getFullName" (CXModule.t @-> returning CXString.t)


  let clang_is_reference = foreign "clang_isReference" (CXCursorKind.t @-> returning uint)

  let clang_visit_children_with_block =
    foreign
      "clang_visitChildrenWithBlock"
      (CXCursor.t @-> CXCursorVisitorBlock.t @-> returning uint)


  let clang_get_translation_unit_spelling =
    foreign
      "clang_getTranslationUnitSpelling"
      (CXTranslationUnit.t @-> returning CXString.t)


  let clang_cursor_get_obj_cproperty_setter_name =
    foreign "clang_Cursor_getObjCPropertySetterName" (CXCursor.t @-> returning CXString.t)


  let clang_tokenize =
    foreign
      "clang_tokenize"
      (CXTranslationUnit.t
       @-> CXSourceRange.t
       @-> ptr (ptr CXToken.t)
       @-> ptr uint
       @-> returning void)


  let clang_cursor_get_template_argument_kind =
    foreign
      "clang_Cursor_getTemplateArgumentKind"
      (CXCursor.t @-> uint @-> returning CXTemplateArgumentKind.t)


  let clang_remap_get_num_files =
    foreign "clang_remap_getNumFiles" (CXRemapping.t @-> returning uint)


  let clang_cursor_get_cxxmanglings =
    foreign "clang_Cursor_getCXXManglings" (CXCursor.t @-> returning (ptr CXStringSet.t))


  let clang_get_cursor_completion_string =
    foreign
      "clang_getCursorCompletionString"
      (CXCursor.t @-> returning CXCompletionString.t)


  let clang_cursor_get_obj_cproperty_attributes =
    foreign
      "clang_Cursor_getObjCPropertyAttributes"
      (CXCursor.t @-> uint @-> returning uint)


  let clang_get_overloaded_decl =
    foreign "clang_getOverloadedDecl" (CXCursor.t @-> uint @-> returning CXCursor.t)


  let clang_get_cursor_semantic_parent =
    foreign "clang_getCursorSemanticParent" (CXCursor.t @-> returning CXCursor.t)


  let clang_suspend_translation_unit =
    foreign "clang_suspendTranslationUnit" (CXTranslationUnit.t @-> returning uint)


  let clang_dispose_string = foreign "clang_disposeString" (CXString.t @-> returning void)

  let clang_eval_result_get_as_str =
    foreign "clang_EvalResult_getAsStr" (CXEvalResult.t @-> returning string)


  let clang_range_is_null =
    foreign "clang_Range_isNull" (CXSourceRange.t @-> returning int)


  let clang_get_specialized_cursor_template =
    foreign "clang_getSpecializedCursorTemplate" (CXCursor.t @-> returning CXCursor.t)


  let clang_get_num_completion_chunks =
    foreign "clang_getNumCompletionChunks" (CXCompletionString.t @-> returning uint)


  let clang_get_token_spelling =
    foreign
      "clang_getTokenSpelling"
      (CXTranslationUnit.t @-> CXToken.t @-> returning CXString.t)


  let clang_cursor_is_inline_namespace =
    foreign "clang_Cursor_isInlineNamespace" (CXCursor.t @-> returning uint)


  let clang_target_info_get_pointer_width =
    foreign "clang_TargetInfo_getPointerWidth" (CXTargetInfo.t @-> returning int)


  let clang_get_location_for_offset =
    foreign
      "clang_getLocationForOffset"
      (CXTranslationUnit.t @-> CXFile.t @-> uint @-> returning CXSourceLocation.t)


  let clang_execute_on_thread =
    foreign
      "clang_executeOnThread"
      (static_funptr Ctypes.(ptr void @-> returning void)
       @-> ptr void
       @-> uint
       @-> returning void)


  let clang_construct_usr_obj_cclass =
    foreign "clang_constructUSR_ObjCClass" (string @-> returning CXString.t)


  let clang_parse_translation_unit2_full_argv =
    foreign
      "clang_parseTranslationUnit2FullArgv"
      (CXIndex.t
       @-> string
       @-> ptr string
       @-> int
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> uint
       @-> ptr CXTranslationUnit.t
       @-> returning CXErrorCode.t)


  let clang_get_cursor_reference_name_range =
    foreign
      "clang_getCursorReferenceNameRange"
      (CXCursor.t @-> uint @-> uint @-> returning CXSourceRange.t)


  let clang_cursor_get_receiver_type =
    foreign "clang_Cursor_getReceiverType" (CXCursor.t @-> returning CXType.t)


  let clang_get_typedef_decl_underlying_type =
    foreign "clang_getTypedefDeclUnderlyingType" (CXCursor.t @-> returning CXType.t)


  let clang_get_remappings_from_file_list =
    foreign
      "clang_getRemappingsFromFileList"
      (ptr string @-> uint @-> returning CXRemapping.t)


  let clang_cxindex_set_invocation_emission_path_option =
    foreign
      "clang_CXIndex_setInvocationEmissionPathOption"
      (CXIndex.t @-> string @-> returning void)


  let clang_code_complete_get_diagnostic =
    foreign
      "clang_codeCompleteGetDiagnostic"
      (ptr CXCodeCompleteResults.t @-> uint @-> returning CXDiagnostic.t)


  let clang_get_template_cursor_kind =
    foreign "clang_getTemplateCursorKind" (CXCursor.t @-> returning CXCursorKind.t)


  let clang_get_canonical_cursor =
    foreign "clang_getCanonicalCursor" (CXCursor.t @-> returning CXCursor.t)


  let clang_get_remappings =
    foreign "clang_getRemappings" (string @-> returning CXRemapping.t)


  let clang_get_enum_constant_decl_value =
    foreign "clang_getEnumConstantDeclValue" (CXCursor.t @-> returning llong)


  let clang_module_get_parent =
    foreign "clang_Module_getParent" (CXModule.t @-> returning CXModule.t)


  let clang_get_unqualified_type =
    foreign "clang_getUnqualifiedType" (CXType.t @-> returning CXType.t)


  let clang_get_range_end =
    foreign "clang_getRangeEnd" (CXSourceRange.t @-> returning CXSourceLocation.t)


  let clang_get_completion_parent =
    foreign
      "clang_getCompletionParent"
      (CXCompletionString.t @-> ptr CXCursorKind.t @-> returning CXString.t)


  let clang_code_complete_at =
    foreign
      "clang_codeCompleteAt"
      (CXTranslationUnit.t
       @-> string
       @-> uint
       @-> uint
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> uint
       @-> returning (ptr CXCodeCompleteResults.t))


  let clang_cursor_get_storage_class =
    foreign "clang_Cursor_getStorageClass" (CXCursor.t @-> returning CX_StorageClass.t)


  let clang_cxxmethod_is_explicit =
    foreign "clang_CXXMethod_isExplicit" (CXCursor.t @-> returning uint)


  let clang_cursor_is_anonymous_record_decl =
    foreign "clang_Cursor_isAnonymousRecordDecl" (CXCursor.t @-> returning uint)


  let clang_cursor_has_var_decl_external_storage =
    foreign "clang_Cursor_hasVarDeclExternalStorage" (CXCursor.t @-> returning int)


  let clang_get_cursor_tlskind =
    foreign "clang_getCursorTLSKind" (CXCursor.t @-> returning CXTLSKind.t)


  let clang_get_cursor_referenced =
    foreign "clang_getCursorReferenced" (CXCursor.t @-> returning CXCursor.t)


  let clang_get_num_elements =
    foreign "clang_getNumElements" (CXType.t @-> returning llong)


  let clang_get_cxturesource_usage =
    foreign
      "clang_getCXTUResourceUsage"
      (CXTranslationUnit.t @-> returning CXTUResourceUsage.t)


  let clang_get_function_type_calling_conv =
    foreign "clang_getFunctionTypeCallingConv" (CXType.t @-> returning CXCallingConv.t)


  let clang_create_translation_unit2 =
    foreign
      "clang_createTranslationUnit2"
      (CXIndex.t @-> string @-> ptr CXTranslationUnit.t @-> returning CXErrorCode.t)


  let clang_location_is_in_system_header =
    foreign "clang_Location_isInSystemHeader" (CXSourceLocation.t @-> returning int)


  let clang_dispose_code_complete_results =
    foreign
      "clang_disposeCodeCompleteResults"
      (ptr CXCodeCompleteResults.t @-> returning void)


  let clang_cursor_get_binary_opcode_str =
    foreign
      "clang_Cursor_getBinaryOpcodeStr"
      (CX_BinaryOperatorKind.t @-> returning CXString.t)


  let clang_get_token_kind =
    foreign "clang_getTokenKind" (CXToken.t @-> returning CXTokenKind.t)


  let clang_get_cursor =
    foreign
      "clang_getCursor"
      (CXTranslationUnit.t @-> CXSourceLocation.t @-> returning CXCursor.t)


  let clang_is_unexposed = foreign "clang_isUnexposed" (CXCursorKind.t @-> returning uint)

  let clang_cxindex_set_global_options =
    foreign "clang_CXIndex_setGlobalOptions" (CXIndex.t @-> uint @-> returning void)


  let clang_type_get_offset_of =
    foreign "clang_Type_getOffsetOf" (CXType.t @-> string @-> returning llong)


  let clang_code_complete_get_obj_cselector =
    foreign
      "clang_codeCompleteGetObjCSelector"
      (ptr CXCodeCompleteResults.t @-> returning CXString.t)


  let clang_index_loc_get_cxsource_location =
    foreign
      "clang_indexLoc_getCXSourceLocation"
      (CXIdxLoc.t @-> returning CXSourceLocation.t)


  let clang_cxxmethod_is_const =
    foreign "clang_CXXMethod_isConst" (CXCursor.t @-> returning uint)


  let clang_get_array_size = foreign "clang_getArraySize" (CXType.t @-> returning llong)

  let clang_parse_translation_unit =
    foreign
      "clang_parseTranslationUnit"
      (CXIndex.t
       @-> string
       @-> ptr string
       @-> int
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> uint
       @-> returning CXTranslationUnit.t)


  let clang_cursor_get_raw_comment_text =
    foreign "clang_Cursor_getRawCommentText" (CXCursor.t @-> returning CXString.t)


  let clang_get_range =
    foreign
      "clang_getRange"
      (CXSourceLocation.t @-> CXSourceLocation.t @-> returning CXSourceRange.t)


  let clang_cursor_get_template_argument_type =
    foreign
      "clang_Cursor_getTemplateArgumentType"
      (CXCursor.t @-> uint @-> returning CXType.t)


  let clang_get_location =
    foreign
      "clang_getLocation"
      (CXTranslationUnit.t @-> CXFile.t @-> uint @-> uint @-> returning CXSourceLocation.t)


  let clang_eval_result_get_as_int =
    foreign "clang_EvalResult_getAsInt" (CXEvalResult.t @-> returning int)


  let clang_get_enum_constant_decl_unsigned_value =
    foreign "clang_getEnumConstantDeclUnsignedValue" (CXCursor.t @-> returning ullong)


  let clang_index_get_obj_ccategory_decl_info =
    foreign
      "clang_index_getObjCCategoryDeclInfo"
      (ptr (const CXIdxDeclInfo.t) @-> returning (ptr (const CXIdxObjCCategoryDeclInfo.t)))


  let clang_get_completion_fix_it =
    foreign
      "clang_getCompletionFixIt"
      (ptr CXCodeCompleteResults.t
       @-> uint
       @-> uint
       @-> ptr CXSourceRange.t
       @-> returning CXString.t)


  let clang_type_get_obj_cobject_base_type =
    foreign "clang_Type_getObjCObjectBaseType" (CXType.t @-> returning CXType.t)


  let clang_get_num_diagnostics =
    foreign "clang_getNumDiagnostics" (CXTranslationUnit.t @-> returning uint)


  let clang_get_field_decl_bit_width =
    foreign "clang_getFieldDeclBitWidth" (CXCursor.t @-> returning int)


  let clang_get_completion_num_fix_its =
    foreign
      "clang_getCompletionNumFixIts"
      (ptr CXCodeCompleteResults.t @-> uint @-> returning uint)


  let clang_type_get_obj_cprotocol_decl =
    foreign "clang_Type_getObjCProtocolDecl" (CXType.t @-> uint @-> returning CXCursor.t)


  let clang_dispose_cxturesource_usage =
    foreign "clang_disposeCXTUResourceUsage" (CXTUResourceUsage.t @-> returning void)


  let clang_index_get_obj_cproperty_decl_info =
    foreign
      "clang_index_getObjCPropertyDeclInfo"
      (ptr (const CXIdxDeclInfo.t) @-> returning (ptr (const CXIdxObjCPropertyDeclInfo.t)))


  let clang_index_is_entity_obj_ccontainer_kind =
    foreign "clang_index_isEntityObjCContainerKind" (CXIdxEntityKind.t @-> returning int)


  let clang_cxcursor_set_contains =
    foreign "clang_CXCursorSet_contains" (CXCursorSet.t @-> CXCursor.t @-> returning uint)


  let clang_construct_usr_obj_cmethod =
    foreign
      "clang_constructUSR_ObjCMethod"
      (string @-> uint @-> CXString.t @-> returning CXString.t)


  let clang_reparse_translation_unit =
    foreign
      "clang_reparseTranslationUnit"
      (CXTranslationUnit.t @-> uint @-> ptr CXUnsavedFile.t @-> uint @-> returning int)


  let clang_index_set_client_container =
    foreign
      "clang_index_setClientContainer"
      (ptr (const CXIdxContainerInfo.t) @-> CXIdxClientContainer.t @-> returning void)


  let clang_cursor_has_var_decl_global_storage =
    foreign "clang_Cursor_hasVarDeclGlobalStorage" (CXCursor.t @-> returning int)


  let clang_get_cursor_kind_spelling =
    foreign "clang_getCursorKindSpelling" (CXCursorKind.t @-> returning CXString.t)


  let clang_cursor_get_var_decl_initializer =
    foreign "clang_Cursor_getVarDeclInitializer" (CXCursor.t @-> returning CXCursor.t)


  let clang_cxxconstructor_is_converting_constructor =
    foreign "clang_CXXConstructor_isConvertingConstructor" (CXCursor.t @-> returning uint)


  let clang_get_cxxaccess_specifier =
    foreign
      "clang_getCXXAccessSpecifier"
      (CXCursor.t @-> returning CX_CXXAccessSpecifier.t)


  let clang_default_code_complete_options =
    foreign "clang_defaultCodeCompleteOptions" (void @-> returning uint)


  let clang_get_unary_operator_kind_spelling =
    foreign
      "clang_getUnaryOperatorKindSpelling"
      (CXUnaryOperatorKind.t @-> returning CXString.t)


  let clang_annotate_tokens =
    foreign
      "clang_annotateTokens"
      (CXTranslationUnit.t
       @-> ptr CXToken.t
       @-> uint
       @-> ptr CXCursor.t
       @-> returning void)


  let clang_equal_ranges =
    foreign "clang_equalRanges" (CXSourceRange.t @-> CXSourceRange.t @-> returning uint)


  let clang_get_token =
    foreign
      "clang_getToken"
      (CXTranslationUnit.t @-> CXSourceLocation.t @-> returning (ptr CXToken.t))


  let clang_equal_types =
    foreign "clang_equalTypes" (CXType.t @-> CXType.t @-> returning uint)


  let clang_create_cxcursor_set =
    foreign "clang_createCXCursorSet" (void @-> returning CXCursorSet.t)


  let clang_eval_result_get_as_unsigned =
    foreign "clang_EvalResult_getAsUnsigned" (CXEvalResult.t @-> returning ullong)


  let clang_cursor_evaluate =
    foreign "clang_Cursor_Evaluate" (CXCursor.t @-> returning CXEvalResult.t)


  let clang_visit_children =
    foreign
      "clang_visitChildren"
      (CXCursor.t @-> CXCursorVisitor.t @-> CXClientData.t @-> returning uint)


  let clang_code_complete_get_container_usr =
    foreign
      "clang_codeCompleteGetContainerUSR"
      (ptr CXCodeCompleteResults.t @-> returning CXString.t)


  let clang_cursor_is_obj_coptional =
    foreign "clang_Cursor_isObjCOptional" (CXCursor.t @-> returning uint)


  let clang_get_element_type =
    foreign "clang_getElementType" (CXType.t @-> returning CXType.t)


  let clang_cursor_is_bit_field =
    foreign "clang_Cursor_isBitField" (CXCursor.t @-> returning uint)


  let clang_cxxmethod_is_move_assignment_operator =
    foreign "clang_CXXMethod_isMoveAssignmentOperator" (CXCursor.t @-> returning uint)


  let clang_cursor_get_comment_range =
    foreign "clang_Cursor_getCommentRange" (CXCursor.t @-> returning CXSourceRange.t)


  let clang_get_inclusions =
    foreign
      "clang_getInclusions"
      (CXTranslationUnit.t @-> CXInclusionVisitor.t @-> CXClientData.t @-> returning void)


  let clang_get_token_location =
    foreign
      "clang_getTokenLocation"
      (CXTranslationUnit.t @-> CXToken.t @-> returning CXSourceLocation.t)


  let clang_is_cursor_definition =
    foreign "clang_isCursorDefinition" (CXCursor.t @-> returning uint)


  let clang_get_result_type =
    foreign "clang_getResultType" (CXType.t @-> returning CXType.t)


  let clang_index_get_obj_ccontainer_decl_info =
    foreign
      "clang_index_getObjCContainerDeclInfo"
      (ptr (const CXIdxDeclInfo.t)
       @-> returning (ptr (const CXIdxObjCContainerDeclInfo.t)))


  let clang_eval_result_get_as_double =
    foreign "clang_EvalResult_getAsDouble" (CXEvalResult.t @-> returning double)


  let clang_get_cursor_unary_operator_kind =
    foreign
      "clang_getCursorUnaryOperatorKind"
      (CXCursor.t @-> returning CXUnaryOperatorKind.t)


  let clang_eval_result_is_unsigned_int =
    foreign "clang_EvalResult_isUnsignedInt" (CXEvalResult.t @-> returning uint)


  let clang_get_exception_specification_type =
    foreign "clang_getExceptionSpecificationType" (CXType.t @-> returning int)


  let clang_dispose_source_range_list =
    foreign "clang_disposeSourceRangeList" (ptr CXSourceRangeList.t @-> returning void)


  let clang_get_cursor_extent =
    foreign "clang_getCursorExtent" (CXCursor.t @-> returning CXSourceRange.t)


  let clang_index_get_client_container =
    foreign
      "clang_index_getClientContainer"
      (ptr (const CXIdxContainerInfo.t) @-> returning CXIdxClientContainer.t)


  let clang_enable_stack_traces =
    foreign "clang_enableStackTraces" (void @-> returning void)


  let clang_equal_cursors =
    foreign "clang_equalCursors" (CXCursor.t @-> CXCursor.t @-> returning uint)


  let clang_cxxmethod_is_defaulted =
    foreign "clang_CXXMethod_isDefaulted" (CXCursor.t @-> returning uint)


  let clang_cursor_get_module =
    foreign "clang_Cursor_getModule" (CXCursor.t @-> returning CXModule.t)


  let clang_get_definition_spelling_and_extent =
    foreign
      "clang_getDefinitionSpellingAndExtent"
      (CXCursor.t
       @-> ptr string
       @-> ptr string
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_default_editing_translation_unit_options =
    foreign "clang_defaultEditingTranslationUnitOptions" (void @-> returning uint)


  let clang_module_get_name =
    foreign "clang_Module_getName" (CXModule.t @-> returning CXString.t)


  let clang_get_cursor_pretty_printed =
    foreign
      "clang_getCursorPrettyPrinted"
      (CXCursor.t @-> CXPrintingPolicy.t @-> returning CXString.t)


  let clang_index_action_dispose =
    foreign "clang_IndexAction_dispose" (CXIndexAction.t @-> returning void)


  let clang_get_cursor_spelling =
    foreign "clang_getCursorSpelling" (CXCursor.t @-> returning CXString.t)


  let clang_index_translation_unit =
    foreign
      "clang_indexTranslationUnit"
      (CXIndexAction.t
       @-> CXClientData.t
       @-> ptr IndexerCallbacks.t
       @-> uint
       @-> uint
       @-> CXTranslationUnit.t
       @-> returning int)


  let clang_cursor_get_obj_cdecl_qualifiers =
    foreign "clang_Cursor_getObjCDeclQualifiers" (CXCursor.t @-> returning uint)


  let clang_get_enum_decl_integer_type =
    foreign "clang_getEnumDeclIntegerType" (CXCursor.t @-> returning CXType.t)


  let clang_get_iboutlet_collection_type =
    foreign "clang_getIBOutletCollectionType" (CXCursor.t @-> returning CXType.t)


  let clang_toggle_crash_recovery =
    foreign "clang_toggleCrashRecovery" (uint @-> returning void)


  let clang_target_info_get_triple =
    foreign "clang_TargetInfo_getTriple" (CXTargetInfo.t @-> returning CXString.t)


  let clang_index_source_file =
    foreign
      "clang_indexSourceFile"
      (CXIndexAction.t
       @-> CXClientData.t
       @-> ptr IndexerCallbacks.t
       @-> uint
       @-> uint
       @-> string
       @-> ptr string
       @-> int
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> ptr CXTranslationUnit.t
       @-> uint
       @-> returning int)


  let clang_code_complete_get_container_kind =
    foreign
      "clang_codeCompleteGetContainerKind"
      (ptr CXCodeCompleteResults.t @-> ptr uint @-> returning CXCursorKind.t)


  let clang_get_num_arg_types = foreign "clang_getNumArgTypes" (CXType.t @-> returning int)

  let clang_get_expansion_location =
    foreign
      "clang_getExpansionLocation"
      (CXSourceLocation.t
       @-> ptr CXFile.t
       @-> ptr uint
       @-> ptr uint
       @-> ptr uint
       @-> returning void)


  let clang_get_cursor_platform_availability =
    foreign
      "clang_getCursorPlatformAvailability"
      (CXCursor.t
       @-> ptr int
       @-> ptr CXString.t
       @-> ptr int
       @-> ptr CXString.t
       @-> ptr CXPlatformAvailability.t
       @-> int
       @-> returning int)


  let clang_get_cursor_result_type =
    foreign "clang_getCursorResultType" (CXCursor.t @-> returning CXType.t)


  let clang_default_save_options =
    foreign "clang_defaultSaveOptions" (CXTranslationUnit.t @-> returning uint)


  let clang_index_source_file_full_argv =
    foreign
      "clang_indexSourceFileFullArgv"
      (CXIndexAction.t
       @-> CXClientData.t
       @-> ptr IndexerCallbacks.t
       @-> uint
       @-> uint
       @-> string
       @-> ptr string
       @-> int
       @-> ptr CXUnsavedFile.t
       @-> uint
       @-> ptr CXTranslationUnit.t
       @-> uint
       @-> returning int)


  let clang_is_invalid = foreign "clang_isInvalid" (CXCursorKind.t @-> returning uint)

  let clang_get_token_extent =
    foreign
      "clang_getTokenExtent"
      (CXTranslationUnit.t @-> CXToken.t @-> returning CXSourceRange.t)


  let clang_dispose_index = foreign "clang_disposeIndex" (CXIndex.t @-> returning void)

  let clang_cursor_is_function_inlined =
    foreign "clang_Cursor_isFunctionInlined" (CXCursor.t @-> returning uint)


  let clang_cursor_get_num_arguments =
    foreign "clang_Cursor_getNumArguments" (CXCursor.t @-> returning int)


  let clang_get_translation_unit_cursor =
    foreign "clang_getTranslationUnitCursor" (CXTranslationUnit.t @-> returning CXCursor.t)


  let clang_cursor_is_external_symbol =
    foreign
      "clang_Cursor_isExternalSymbol"
      (CXCursor.t @-> ptr CXString.t @-> ptr CXString.t @-> ptr uint @-> returning uint)


  let clang_printing_policy_set_property =
    foreign
      "clang_PrintingPolicy_setProperty"
      (CXPrintingPolicy.t @-> CXPrintingPolicyProperty.t @-> uint @-> returning void)


  let clang_get_completion_chunk_completion_string =
    foreign
      "clang_getCompletionChunkCompletionString"
      (CXCompletionString.t @-> uint @-> returning CXCompletionString.t)


  let clang_index_get_cxxclass_decl_info =
    foreign
      "clang_index_getCXXClassDeclInfo"
      (ptr (const CXIdxDeclInfo.t) @-> returning (ptr (const CXIdxCXXClassDeclInfo.t)))


  let clang_get_completion_chunk_kind =
    foreign
      "clang_getCompletionChunkKind"
      (CXCompletionString.t @-> uint @-> returning CXCompletionChunkKind.t)


  let clang_module_get_num_top_level_headers =
    foreign
      "clang_Module_getNumTopLevelHeaders"
      (CXTranslationUnit.t @-> CXModule.t @-> returning uint)


  let clang_cursor_get_brief_comment_text =
    foreign "clang_Cursor_getBriefCommentText" (CXCursor.t @-> returning CXString.t)
end

open Ctypes

open Bindings.Types
open Bindings.Functions

type index = CXIndex.t
type translation_unit = CXTranslationUnit.t
type client_data = CXClientData.t
type unsaved_file = CXUnsavedFile.t structure
type cursor = CXCursor.t structure
type source_location = CXSourceLocation.t structure
type ctype = CXType.t structure
type cursor_kind = CXCursorKind.t =
  | CXCursor_UnexposedDecl
  | CXCursor_StructDecl
  | CXCursor_UnionDecl
  | CXCursor_ClassDecl
  | CXCursor_EnumDecl
  | CXCursor_FieldDecl
  | CXCursor_EnumConstantDecl
  | CXCursor_FunctionDecl
  | CXCursor_VarDecl
  | CXCursor_ParmDecl
  | CXCursor_ObjCInterfaceDecl
  | CXCursor_ObjCCategoryDecl
  | CXCursor_ObjCProtocolDecl
  | CXCursor_ObjCPropertyDecl
  | CXCursor_ObjCIvarDecl
  | CXCursor_ObjCInstanceMethodDecl
  | CXCursor_ObjCClassMethodDecl
  | CXCursor_ObjCImplementationDecl
  | CXCursor_ObjCCategoryImplDecl
  | CXCursor_TypedefDecl
  | CXCursor_CXXMethod
  | CXCursor_Namespace
  | CXCursor_LinkageSpec
  | CXCursor_Constructor
  | CXCursor_Destructor
  | CXCursor_ConversionFunction
  | CXCursor_TemplateTypeParameter
  | CXCursor_NonTypeTemplateParameter
  | CXCursor_TemplateTemplateParameter
  | CXCursor_FunctionTemplate
  | CXCursor_ClassTemplate
  | CXCursor_ClassTemplatePartialSpecialization
  | CXCursor_NamespaceAlias
  | CXCursor_UsingDirective
  | CXCursor_UsingDeclaration
  | CXCursor_TypeAliasDecl
  | CXCursor_ObjCSynthesizeDecl
  | CXCursor_ObjCDynamicDecl
  | CXCursor_CXXAccessSpecifier
  | CXCursor_FirstDecl
  | CXCursor_LastDecl
  | CXCursor_FirstRef
  | CXCursor_ObjCSuperClassRef
  | CXCursor_ObjCProtocolRef
  | CXCursor_ObjCClassRef
  | CXCursor_TypeRef
  | CXCursor_CXXBaseSpecifier
  | CXCursor_TemplateRef
  | CXCursor_NamespaceRef
  | CXCursor_MemberRef
  | CXCursor_LabelRef
  | CXCursor_OverloadedDeclRef
  | CXCursor_VariableRef
  | CXCursor_LastRef
  | CXCursor_FirstInvalid
  | CXCursor_InvalidFile
  | CXCursor_NoDeclFound
  | CXCursor_NotImplemented
  | CXCursor_InvalidCode
  | CXCursor_LastInvalid
  | CXCursor_FirstExpr
  | CXCursor_UnexposedExpr
  | CXCursor_DeclRefExpr
  | CXCursor_MemberRefExpr
  | CXCursor_CallExpr
  | CXCursor_ObjCMessageExpr
  | CXCursor_BlockExpr
  | CXCursor_IntegerLiteral
  | CXCursor_FloatingLiteral
  | CXCursor_ImaginaryLiteral
  | CXCursor_StringLiteral
  | CXCursor_CharacterLiteral
  | CXCursor_ParenExpr
  | CXCursor_UnaryOperator
  | CXCursor_ArraySubscriptExpr
  | CXCursor_BinaryOperator
  | CXCursor_CompoundAssignOperator
  | CXCursor_ConditionalOperator
  | CXCursor_CStyleCastExpr
  | CXCursor_CompoundLiteralExpr
  | CXCursor_InitListExpr
  | CXCursor_AddrLabelExpr
  | CXCursor_StmtExpr
  | CXCursor_GenericSelectionExpr
  | CXCursor_GNUNullExpr
  | CXCursor_CXXStaticCastExpr
  | CXCursor_CXXDynamicCastExpr
  | CXCursor_CXXReinterpretCastExpr
  | CXCursor_CXXConstCastExpr
  | CXCursor_CXXFunctionalCastExpr
  | CXCursor_CXXTypeidExpr
  | CXCursor_CXXBoolLiteralExpr
  | CXCursor_CXXNullPtrLiteralExpr
  | CXCursor_CXXThisExpr
  | CXCursor_CXXThrowExpr
  | CXCursor_CXXNewExpr
  | CXCursor_CXXDeleteExpr
  | CXCursor_UnaryExpr
  | CXCursor_ObjCStringLiteral
  | CXCursor_ObjCEncodeExpr
  | CXCursor_ObjCSelectorExpr
  | CXCursor_ObjCProtocolExpr
  | CXCursor_ObjCBridgedCastExpr
  | CXCursor_PackExpansionExpr
  | CXCursor_SizeOfPackExpr
  | CXCursor_LambdaExpr
  | CXCursor_ObjCBoolLiteralExpr
  | CXCursor_ObjCSelfExpr
  | CXCursor_ArraySectionExpr
  | CXCursor_ObjCAvailabilityCheckExpr
  | CXCursor_FixedPointLiteral
  | CXCursor_OMPArrayShapingExpr
  | CXCursor_OMPIteratorExpr
  | CXCursor_CXXAddrspaceCastExpr
  | CXCursor_ConceptSpecializationExpr
  | CXCursor_RequiresExpr
  | CXCursor_CXXParenListInitExpr
  | CXCursor_PackIndexingExpr
  | CXCursor_LastExpr
  | CXCursor_FirstStmt
  | CXCursor_UnexposedStmt
  | CXCursor_LabelStmt
  | CXCursor_CompoundStmt
  | CXCursor_CaseStmt
  | CXCursor_DefaultStmt
  | CXCursor_IfStmt
  | CXCursor_SwitchStmt
  | CXCursor_WhileStmt
  | CXCursor_DoStmt
  | CXCursor_ForStmt
  | CXCursor_GotoStmt
  | CXCursor_IndirectGotoStmt
  | CXCursor_ContinueStmt
  | CXCursor_BreakStmt
  | CXCursor_ReturnStmt
  | CXCursor_GCCAsmStmt
  | CXCursor_AsmStmt
  | CXCursor_ObjCAtTryStmt
  | CXCursor_ObjCAtCatchStmt
  | CXCursor_ObjCAtFinallyStmt
  | CXCursor_ObjCAtThrowStmt
  | CXCursor_ObjCAtSynchronizedStmt
  | CXCursor_ObjCAutoreleasePoolStmt
  | CXCursor_ObjCForCollectionStmt
  | CXCursor_CXXCatchStmt
  | CXCursor_CXXTryStmt
  | CXCursor_CXXForRangeStmt
  | CXCursor_SEHTryStmt
  | CXCursor_SEHExceptStmt
  | CXCursor_SEHFinallyStmt
  | CXCursor_MSAsmStmt
  | CXCursor_NullStmt
  | CXCursor_DeclStmt
  | CXCursor_OMPParallelDirective
  | CXCursor_OMPSimdDirective
  | CXCursor_OMPForDirective
  | CXCursor_OMPSectionsDirective
  | CXCursor_OMPSectionDirective
  | CXCursor_OMPSingleDirective
  | CXCursor_OMPParallelForDirective
  | CXCursor_OMPParallelSectionsDirective
  | CXCursor_OMPTaskDirective
  | CXCursor_OMPMasterDirective
  | CXCursor_OMPCriticalDirective
  | CXCursor_OMPTaskyieldDirective
  | CXCursor_OMPBarrierDirective
  | CXCursor_OMPTaskwaitDirective
  | CXCursor_OMPFlushDirective
  | CXCursor_SEHLeaveStmt
  | CXCursor_OMPOrderedDirective
  | CXCursor_OMPAtomicDirective
  | CXCursor_OMPForSimdDirective
  | CXCursor_OMPParallelForSimdDirective
  | CXCursor_OMPTargetDirective
  | CXCursor_OMPTeamsDirective
  | CXCursor_OMPTaskgroupDirective
  | CXCursor_OMPCancellationPointDirective
  | CXCursor_OMPCancelDirective
  | CXCursor_OMPTargetDataDirective
  | CXCursor_OMPTaskLoopDirective
  | CXCursor_OMPTaskLoopSimdDirective
  | CXCursor_OMPDistributeDirective
  | CXCursor_OMPTargetEnterDataDirective
  | CXCursor_OMPTargetExitDataDirective
  | CXCursor_OMPTargetParallelDirective
  | CXCursor_OMPTargetParallelForDirective
  | CXCursor_OMPTargetUpdateDirective
  | CXCursor_OMPDistributeParallelForDirective
  | CXCursor_OMPDistributeParallelForSimdDirective
  | CXCursor_OMPDistributeSimdDirective
  | CXCursor_OMPTargetParallelForSimdDirective
  | CXCursor_OMPTargetSimdDirective
  | CXCursor_OMPTeamsDistributeDirective
  | CXCursor_OMPTeamsDistributeSimdDirective
  | CXCursor_OMPTeamsDistributeParallelForSimdDirective
  | CXCursor_OMPTeamsDistributeParallelForDirective
  | CXCursor_OMPTargetTeamsDirective
  | CXCursor_OMPTargetTeamsDistributeDirective
  | CXCursor_OMPTargetTeamsDistributeParallelForDirective
  | CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective
  | CXCursor_OMPTargetTeamsDistributeSimdDirective
  | CXCursor_BuiltinBitCastExpr
  | CXCursor_OMPMasterTaskLoopDirective
  | CXCursor_OMPParallelMasterTaskLoopDirective
  | CXCursor_OMPMasterTaskLoopSimdDirective
  | CXCursor_OMPParallelMasterTaskLoopSimdDirective
  | CXCursor_OMPParallelMasterDirective
  | CXCursor_OMPDepobjDirective
  | CXCursor_OMPScanDirective
  | CXCursor_OMPTileDirective
  | CXCursor_OMPCanonicalLoop
  | CXCursor_OMPInteropDirective
  | CXCursor_OMPDispatchDirective
  | CXCursor_OMPMaskedDirective
  | CXCursor_OMPUnrollDirective
  | CXCursor_OMPMetaDirective
  | CXCursor_OMPGenericLoopDirective
  | CXCursor_OMPTeamsGenericLoopDirective
  | CXCursor_OMPTargetTeamsGenericLoopDirective
  | CXCursor_OMPParallelGenericLoopDirective
  | CXCursor_OMPTargetParallelGenericLoopDirective
  | CXCursor_OMPParallelMaskedDirective
  | CXCursor_OMPMaskedTaskLoopDirective
  | CXCursor_OMPMaskedTaskLoopSimdDirective
  | CXCursor_OMPParallelMaskedTaskLoopDirective
  | CXCursor_OMPParallelMaskedTaskLoopSimdDirective
  | CXCursor_OMPErrorDirective
  | CXCursor_OMPScopeDirective
  | CXCursor_OMPReverseDirective
  | CXCursor_OMPInterchangeDirective
  | CXCursor_OpenACCComputeConstruct
  | CXCursor_OpenACCLoopConstruct
  | CXCursor_LastStmt
  | CXCursor_TranslationUnit
  | CXCursor_FirstAttr
  | CXCursor_UnexposedAttr
  | CXCursor_IBActionAttr
  | CXCursor_IBOutletAttr
  | CXCursor_IBOutletCollectionAttr
  | CXCursor_CXXFinalAttr
  | CXCursor_CXXOverrideAttr
  | CXCursor_AnnotateAttr
  | CXCursor_AsmLabelAttr
  | CXCursor_PackedAttr
  | CXCursor_PureAttr
  | CXCursor_ConstAttr
  | CXCursor_NoDuplicateAttr
  | CXCursor_CUDAConstantAttr
  | CXCursor_CUDADeviceAttr
  | CXCursor_CUDAGlobalAttr
  | CXCursor_CUDAHostAttr
  | CXCursor_CUDASharedAttr
  | CXCursor_VisibilityAttr
  | CXCursor_DLLExport
  | CXCursor_DLLImport
  | CXCursor_NSReturnsRetained
  | CXCursor_NSReturnsNotRetained
  | CXCursor_NSReturnsAutoreleased
  | CXCursor_NSConsumesSelf
  | CXCursor_NSConsumed
  | CXCursor_ObjCException
  | CXCursor_ObjCNSObject
  | CXCursor_ObjCIndependentClass
  | CXCursor_ObjCPreciseLifetime
  | CXCursor_ObjCReturnsInnerPointer
  | CXCursor_ObjCRequiresSuper
  | CXCursor_ObjCRootClass
  | CXCursor_ObjCSubclassingRestricted
  | CXCursor_ObjCExplicitProtocolImpl
  | CXCursor_ObjCDesignatedInitializer
  | CXCursor_ObjCRuntimeVisible
  | CXCursor_ObjCBoxable
  | CXCursor_FlagEnum
  | CXCursor_ConvergentAttr
  | CXCursor_WarnUnusedAttr
  | CXCursor_WarnUnusedResultAttr
  | CXCursor_AlignedAttr
  | CXCursor_LastAttr
  | CXCursor_PreprocessingDirective
  | CXCursor_MacroDefinition
  | CXCursor_MacroExpansion
  | CXCursor_MacroInstantiation
  | CXCursor_InclusionDirective
  | CXCursor_FirstPreprocessing
  | CXCursor_LastPreprocessing
  | CXCursor_ModuleImportDecl
  | CXCursor_TypeAliasTemplateDecl
  | CXCursor_StaticAssert
  | CXCursor_FriendDecl
  | CXCursor_ConceptDecl
  | CXCursor_FirstExtraDecl
  | CXCursor_LastExtraDecl
  | CXCursor_OverloadCandidate
type type_kind = CXTypeKind.t =
  | CXType_Invalid
  | CXType_Unexposed
  | CXType_Void
  | CXType_Bool
  | CXType_Char_U
  | CXType_UChar
  | CXType_Char16
  | CXType_Char32
  | CXType_UShort
  | CXType_UInt
  | CXType_ULong
  | CXType_ULongLong
  | CXType_UInt128
  | CXType_Char_S
  | CXType_SChar
  | CXType_WChar
  | CXType_Short
  | CXType_Int
  | CXType_Long
  | CXType_LongLong
  | CXType_Int128
  | CXType_Float
  | CXType_Double
  | CXType_LongDouble
  | CXType_NullPtr
  | CXType_Overload
  | CXType_Dependent
  | CXType_ObjCId
  | CXType_ObjCClass
  | CXType_ObjCSel
  | CXType_Float128
  | CXType_Half
  | CXType_Float16
  | CXType_ShortAccum
  | CXType_Accum
  | CXType_LongAccum
  | CXType_UShortAccum
  | CXType_UAccum
  | CXType_ULongAccum
  | CXType_BFloat16
  | CXType_Ibm128
  | CXType_FirstBuiltin
  | CXType_LastBuiltin
  | CXType_Complex
  | CXType_Pointer
  | CXType_BlockPointer
  | CXType_LValueReference
  | CXType_RValueReference
  | CXType_Record
  | CXType_Enum
  | CXType_Typedef
  | CXType_ObjCInterface
  | CXType_ObjCObjectPointer
  | CXType_FunctionNoProto
  | CXType_FunctionProto
  | CXType_ConstantArray
  | CXType_Vector
  | CXType_IncompleteArray
  | CXType_VariableArray
  | CXType_DependentSizedArray
  | CXType_MemberPointer
  | CXType_Auto
  | CXType_Elaborated
  | CXType_Pipe
  | CXType_OCLImage1dRO
  | CXType_OCLImage1dArrayRO
  | CXType_OCLImage1dBufferRO
  | CXType_OCLImage2dRO
  | CXType_OCLImage2dArrayRO
  | CXType_OCLImage2dDepthRO
  | CXType_OCLImage2dArrayDepthRO
  | CXType_OCLImage2dMSAARO
  | CXType_OCLImage2dArrayMSAARO
  | CXType_OCLImage2dMSAADepthRO
  | CXType_OCLImage2dArrayMSAADepthRO
  | CXType_OCLImage3dRO
  | CXType_OCLImage1dWO
  | CXType_OCLImage1dArrayWO
  | CXType_OCLImage1dBufferWO
  | CXType_OCLImage2dWO
  | CXType_OCLImage2dArrayWO
  | CXType_OCLImage2dDepthWO
  | CXType_OCLImage2dArrayDepthWO
  | CXType_OCLImage2dMSAAWO
  | CXType_OCLImage2dArrayMSAAWO
  | CXType_OCLImage2dMSAADepthWO
  | CXType_OCLImage2dArrayMSAADepthWO
  | CXType_OCLImage3dWO
  | CXType_OCLImage1dRW
  | CXType_OCLImage1dArrayRW
  | CXType_OCLImage1dBufferRW
  | CXType_OCLImage2dRW
  | CXType_OCLImage2dArrayRW
  | CXType_OCLImage2dDepthRW
  | CXType_OCLImage2dArrayDepthRW
  | CXType_OCLImage2dMSAARW
  | CXType_OCLImage2dArrayMSAARW
  | CXType_OCLImage2dMSAADepthRW
  | CXType_OCLImage2dArrayMSAADepthRW
  | CXType_OCLImage3dRW
  | CXType_OCLSampler
  | CXType_OCLEvent
  | CXType_OCLQueue
  | CXType_OCLReserveID
  | CXType_ObjCObject
  | CXType_ObjCTypeParam
  | CXType_Attributed
  | CXType_OCLIntelSubgroupAVCMcePayload
  | CXType_OCLIntelSubgroupAVCImePayload
  | CXType_OCLIntelSubgroupAVCRefPayload
  | CXType_OCLIntelSubgroupAVCSicPayload
  | CXType_OCLIntelSubgroupAVCMceResult
  | CXType_OCLIntelSubgroupAVCImeResult
  | CXType_OCLIntelSubgroupAVCRefResult
  | CXType_OCLIntelSubgroupAVCSicResult
  | CXType_OCLIntelSubgroupAVCImeResultSingleReferenceStreamout
  | CXType_OCLIntelSubgroupAVCImeResultDualReferenceStreamout
  | CXType_OCLIntelSubgroupAVCImeSingleReferenceStreamin
  | CXType_OCLIntelSubgroupAVCImeDualReferenceStreamin
  | CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout
  | CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout
  | CXType_OCLIntelSubgroupAVCImeSingleRefStreamin
  | CXType_OCLIntelSubgroupAVCImeDualRefStreamin
  | CXType_ExtVector
  | CXType_Atomic
  | CXType_BTFTagAttributed
type child_visit_result = CXChildVisitResult.t =
  | CXChildVisit_Break
  | CXChildVisit_Continue
  | CXChildVisit_Recurse
type translation_unit_flags = CXTranslationUnit_Flags.t =
  | CXTranslationUnit_None
  | CXTranslationUnit_DetailedPreprocessingRecord
  | CXTranslationUnit_Incomplete
  | CXTranslationUnit_PrecompiledPreamble
  | CXTranslationUnit_CacheCompletionResults
  | CXTranslationUnit_ForSerialization
  | CXTranslationUnit_CXXChainedPCH
  | CXTranslationUnit_SkipFunctionBodies
  | CXTranslationUnit_IncludeBriefCommentsInCodeCompletion
  | CXTranslationUnit_CreatePreambleOnFirstParse
  | CXTranslationUnit_KeepGoing
  | CXTranslationUnit_SingleFileParse
  | CXTranslationUnit_LimitSkipFunctionBodiesToPreamble
  | CXTranslationUnit_IncludeAttributedTypes
  | CXTranslationUnit_VisitImplicitAttributes
  | CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles
  | CXTranslationUnit_RetainExcludedConditionalBlocks
module Index = struct

  let create excludeDeclarationsFromPCH displayDiagnostics =
    let result = clang_create_index excludeDeclarationsFromPCH displayDiagnostics in
    if is_null result then None else Some result
  
  let dispose index = clang_dispose_index index

end

module TranslationUnit = struct

  let parse index source_filename command_line_args unsaved_files ~flags =
    let arg_array = CArray.of_list string command_line_args in
    let files_array = CArray.of_list CXUnsavedFile.t unsaved_files in
    let mapping = List.assoc flags CXTranslationUnit_Flags.mapping in
    let result = clang_parse_translation_unit index source_filename (CArray.start arg_array) (CArray.length arg_array) (CArray.start files_array) (Unsigned.UInt.of_int (CArray.length files_array)) (Unsigned.UInt.of_int64 mapping) in
    if Ctypes.is_null result then None else Some result

  let cursor translation_unit = let result = clang_get_translation_unit_cursor translation_unit in
    if clang_cursor_is_null result == 0 then Some result else None

  let dispose translation_unit = clang_dispose_translation_unit translation_unit

end

module Cursor = struct

	let kind cursor = clang_get_cursor_kind cursor

  let location cursor = clang_get_cursor_location cursor

  let ctype cursor = clang_get_cursor_type cursor

  let spelling cursor =
    let cstring = clang_get_cursor_spelling cursor in
    let result = clang_get_cstring cstring in
    clang_dispose_string cstring;
    result

  let visit_children cursor visitor =
    let funptr = coerce
      (Foreign.funptr (CXCursor.t @-> CXCursor.t @-> CXClientData.t @-> returning CXChildVisitResult.t))
      CXCursorVisitor.t
      (fun child _ _ -> visitor child cursor) in
    let _ = clang_visit_children cursor funptr null in ()
  
  let underlying_type cursor =
    if kind cursor = CXCursor_TypedefDecl then
      Some (clang_get_typedef_decl_underlying_type cursor)
    else None

  let is_bit_field cursor = if clang_cursor_is_bit_field cursor = Unsigned.UInt.zero then false else true
end

module SourceLocation = struct

    let is_in_system_header source_location = if clang_location_is_in_system_header source_location == 0 then false else true

    let is_from_main_file source_location = if clang_location_is_from_main_file source_location == 0 then false else true

end

module CType = struct

    let kind ctype = getf ctype CXType.kind

    let is_const_qualified ctype =
      if clang_is_const_qualified_type ctype = Unsigned.UInt.zero then false else true

    let spelling ctype =
      let cstring = clang_get_type_spelling ctype in
      let result = clang_get_cstring cstring in
      clang_dispose_string cstring;
      result

    let declaration ctype = clang_get_type_declaration ctype

    let get_canonical_type ctype =
      let result = clang_get_canonical_type ctype in
      if kind ctype = CXType_Invalid then None else Some result

    let get_return_type ctype =
      let result = clang_get_result_type ctype in
      if kind ctype = CXType_Invalid then None else Some result

    let get_pointee_type ctype =
      let result = clang_get_pointee_type ctype in
      if kind ctype = CXType_Invalid then None else Some result
    
    let get_constant_array_data ctype =
      let element_type = clang_get_array_element_type ctype in
      if kind element_type = CXType_Invalid then None
      else begin
        let size = clang_get_array_size ctype |> Signed.LLong.to_int64 in
        if size = Int64.of_int (-1) then None
        else Some (element_type, size)
      end

end

let with_index excludeDeclarationsFromPCH displayDiagnostics f =
  match Index.create excludeDeclarationsFromPCH displayDiagnostics with
  | Some index -> Fun.protect
    ~finally:(fun _ -> Index.dispose index)
    (fun _ -> f index)
  | None -> failwith "Unable to create an index with Clang!"

let with_translation_unit filename command_line_args f index =
  match TranslationUnit.parse index filename command_line_args [] ~flags:CXTranslationUnit_None with
  | Some translation_unit -> Fun.protect
    ~finally:(fun _ -> TranslationUnit.dispose translation_unit)
    (fun _ -> f translation_unit)
  | None -> failwith (Printf.sprintf "Unable to initialize a translation unit for the file %s!" filename)

let with_cursor f translation_unit =
  match TranslationUnit.cursor translation_unit with
  | Some cursor -> f cursor
  | None -> failwith (Printf.sprintf "Unable to retrieve the cursor from the translation unit!")
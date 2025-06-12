type index
type translation_unit
type client_data
type unsaved_file
type cursor
type source_location
type ctype

type cursor_kind =
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
(* | UnexposedDecl
	| StructDecl
	| EnumDecl
	| FieldDecl
	| EnumConstantDecl
	| FunctionDecl
	| ParmDecl
	| TypedefDecl
	| Unspecified of int64 *)

type type_kind =
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
(* | Invalid
	| Void
	| Bool
	| Char_U
	| UChar
	| UInt
	| ULong
	| ULongLong
	| Char_S
	| SChar
	| Int
	| Long
	| LongLong
	| Float
	| Double
	| Typedef
	| FunctionProto
	| Unexposed
	| Pointer
	| Record
	| Enum
	| ConstantArray
	| Other of int64 default case *)

type child_visit_result =
  | CXChildVisit_Break
  | CXChildVisit_Continue
  | CXChildVisit_Recurse
(* | Break
	| Continue
	| Recurse *)

type translation_unit_flags =
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

module Index : sig
  val create : int -> int -> index option
  val dispose : index -> unit
end

module TranslationUnit : sig
  val parse
    :  index
    -> string
    -> string list
    -> unsaved_file list
    -> flags:translation_unit_flags
    -> translation_unit option

  val cursor : translation_unit -> cursor option
  val dispose : translation_unit -> unit
end

module Cursor : sig
  val kind : cursor -> cursor_kind
  val location : cursor -> source_location
  val ctype : cursor -> ctype
  val spelling : cursor -> string

  (* val visit_children : cursor -> (cursor -> cursor -> 'a ref -> child_visit_result) -> 'a ref -> unit *)
  val visit_children : cursor -> (cursor -> cursor -> child_visit_result) -> unit
  val underlying_type : cursor -> ctype option
  val is_bit_field : cursor -> bool
end

module SourceLocation : sig
  val is_in_system_header : source_location -> bool
  val is_from_main_file : source_location -> bool
end

module CType : sig
  val kind : ctype -> type_kind
  val spelling : ctype -> string
  val declaration : ctype -> cursor
  val is_const_qualified : ctype -> bool
  val get_canonical_type : ctype -> ctype option
  val get_return_type : ctype -> ctype option
  val get_pointee_type : ctype -> ctype option
  val get_constant_array_data : ctype -> (ctype * int64) option
end

val with_index : int -> int -> (index -> 'a) -> 'a

val with_translation_unit
  :  string
  -> string list
  -> (translation_unit -> 'a)
  -> index
  -> 'a

val with_cursor : (cursor -> 'a) -> translation_unit -> 'a

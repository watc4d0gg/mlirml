open Ctypes

module Types (T : TYPE) = struct
  open T

  module CXIndex = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIndex"
  end

  module CXTargetInfo = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXTargetInfo"
  end

  module CXTranslationUnit = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXTranslationUnit"
  end

  module CXClientData = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXClientData"
  end

  module CXUnsavedFile = struct
    type t

    let t : t structure typ = structure "CXUnsavedFile"
    let filename = field t "Filename" string
    let contents = field t "Contents" string
    let length = field t "Length" ulong
    let () = seal t
  end

  module CXAvailabilityKind = struct
    type t =
      | CXAvailability_Available
      | CXAvailability_Deprecated
      | CXAvailability_NotAvailable
      | CXAvailability_NotAccessible

    let mapping =
      [ CXAvailability_Available, constant "CXAvailability_Available" int64_t
      ; CXAvailability_Deprecated, constant "CXAvailability_Deprecated" int64_t
      ; CXAvailability_NotAvailable, constant "CXAvailability_NotAvailable" int64_t
      ; CXAvailability_NotAccessible, constant "CXAvailability_NotAccessible" int64_t
      ]


    let t : t typ =
      enum "CXAvailabilityKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXVersion = struct
    type t

    let t : t structure typ = typedef (structure "CXVersion") "CXVersion"
    let major = field t "Major" int
    let minor = field t "Minor" int
    let subminor = field t "Subminor" int
    let () = seal t
  end

  module CXCursor_ExceptionSpecificationKind = struct
    type t =
      | CXCursor_ExceptionSpecificationKind_None
      | CXCursor_ExceptionSpecificationKind_DynamicNone
      | CXCursor_ExceptionSpecificationKind_Dynamic
      | CXCursor_ExceptionSpecificationKind_MSAny
      | CXCursor_ExceptionSpecificationKind_BasicNoexcept
      | CXCursor_ExceptionSpecificationKind_ComputedNoexcept
      | CXCursor_ExceptionSpecificationKind_Unevaluated
      | CXCursor_ExceptionSpecificationKind_Uninstantiated
      | CXCursor_ExceptionSpecificationKind_Unparsed
      | CXCursor_ExceptionSpecificationKind_NoThrow

    let mapping =
      [ ( CXCursor_ExceptionSpecificationKind_None
        , constant "CXCursor_ExceptionSpecificationKind_None" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_DynamicNone
        , constant "CXCursor_ExceptionSpecificationKind_DynamicNone" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_Dynamic
        , constant "CXCursor_ExceptionSpecificationKind_Dynamic" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_MSAny
        , constant "CXCursor_ExceptionSpecificationKind_MSAny" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_BasicNoexcept
        , constant "CXCursor_ExceptionSpecificationKind_BasicNoexcept" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_ComputedNoexcept
        , constant "CXCursor_ExceptionSpecificationKind_ComputedNoexcept" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_Unevaluated
        , constant "CXCursor_ExceptionSpecificationKind_Unevaluated" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_Uninstantiated
        , constant "CXCursor_ExceptionSpecificationKind_Uninstantiated" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_Unparsed
        , constant "CXCursor_ExceptionSpecificationKind_Unparsed" int64_t )
      ; ( CXCursor_ExceptionSpecificationKind_NoThrow
        , constant "CXCursor_ExceptionSpecificationKind_NoThrow" int64_t )
      ]


    let t : t typ =
      enum
        "CXCursor_ExceptionSpecificationKind"
        ~typedef:false
        mapping
        ~unexpected:(fun _ -> assert false)
  end

  module CXChoice = struct
    type t =
      | CXChoice_Default
      | CXChoice_Enabled
      | CXChoice_Disabled

    let mapping =
      [ CXChoice_Default, constant "CXChoice_Default" int64_t
      ; CXChoice_Enabled, constant "CXChoice_Enabled" int64_t
      ; CXChoice_Disabled, constant "CXChoice_Disabled" int64_t
      ]


    let t : t typ =
      enum "CXChoice" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXGlobalOptFlags = struct
    type t =
      | CXGlobalOpt_None
      | CXGlobalOpt_ThreadBackgroundPriorityForIndexing
      | CXGlobalOpt_ThreadBackgroundPriorityForEditing
      | CXGlobalOpt_ThreadBackgroundPriorityForAll

    let mapping =
      [ CXGlobalOpt_None, constant "CXGlobalOpt_None" int64_t
      ; ( CXGlobalOpt_ThreadBackgroundPriorityForIndexing
        , constant "CXGlobalOpt_ThreadBackgroundPriorityForIndexing" int64_t )
      ; ( CXGlobalOpt_ThreadBackgroundPriorityForEditing
        , constant "CXGlobalOpt_ThreadBackgroundPriorityForEditing" int64_t )
      ; ( CXGlobalOpt_ThreadBackgroundPriorityForAll
        , constant "CXGlobalOpt_ThreadBackgroundPriorityForAll" int64_t )
      ]


    let t : t typ =
      enum "CXGlobalOptFlags" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIndexOptions = struct
    type t

    let t : t structure typ = typedef (structure "CXIndexOptions") "CXIndexOptions"
    let size = field t "Size" uint

    let thread_background_priority_for_indexing =
      field t "ThreadBackgroundPriorityForIndexing" uchar


    let thread_background_priority_for_editing =
      field t "ThreadBackgroundPriorityForEditing" uchar


    let preamble_storage_path = field t "PreambleStoragePath" string
    let invocation_emission_path = field t "InvocationEmissionPath" string
    let () = seal t
  end

  module CXTranslationUnit_Flags = struct
    type t =
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

    let mapping =
      [ CXTranslationUnit_None, constant "CXTranslationUnit_None" int64_t
      ; ( CXTranslationUnit_DetailedPreprocessingRecord
        , constant "CXTranslationUnit_DetailedPreprocessingRecord" int64_t )
      ; CXTranslationUnit_Incomplete, constant "CXTranslationUnit_Incomplete" int64_t
      ; ( CXTranslationUnit_PrecompiledPreamble
        , constant "CXTranslationUnit_PrecompiledPreamble" int64_t )
      ; ( CXTranslationUnit_CacheCompletionResults
        , constant "CXTranslationUnit_CacheCompletionResults" int64_t )
      ; ( CXTranslationUnit_ForSerialization
        , constant "CXTranslationUnit_ForSerialization" int64_t )
      ; ( CXTranslationUnit_CXXChainedPCH
        , constant "CXTranslationUnit_CXXChainedPCH" int64_t )
      ; ( CXTranslationUnit_SkipFunctionBodies
        , constant "CXTranslationUnit_SkipFunctionBodies" int64_t )
      ; ( CXTranslationUnit_IncludeBriefCommentsInCodeCompletion
        , constant "CXTranslationUnit_IncludeBriefCommentsInCodeCompletion" int64_t )
      ; ( CXTranslationUnit_CreatePreambleOnFirstParse
        , constant "CXTranslationUnit_CreatePreambleOnFirstParse" int64_t )
      ; CXTranslationUnit_KeepGoing, constant "CXTranslationUnit_KeepGoing" int64_t
      ; ( CXTranslationUnit_SingleFileParse
        , constant "CXTranslationUnit_SingleFileParse" int64_t )
      ; ( CXTranslationUnit_LimitSkipFunctionBodiesToPreamble
        , constant "CXTranslationUnit_LimitSkipFunctionBodiesToPreamble" int64_t )
      ; ( CXTranslationUnit_IncludeAttributedTypes
        , constant "CXTranslationUnit_IncludeAttributedTypes" int64_t )
      ; ( CXTranslationUnit_VisitImplicitAttributes
        , constant "CXTranslationUnit_VisitImplicitAttributes" int64_t )
      ; ( CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles
        , constant "CXTranslationUnit_IgnoreNonErrorsFromIncludedFiles" int64_t )
      ; ( CXTranslationUnit_RetainExcludedConditionalBlocks
        , constant "CXTranslationUnit_RetainExcludedConditionalBlocks" int64_t )
      ]


    let t : t typ =
      enum "CXTranslationUnit_Flags" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXSaveTranslationUnit_Flags = struct
    type t = CXSaveTranslationUnit_None

    let mapping =
      [ CXSaveTranslationUnit_None, constant "CXSaveTranslationUnit_None" int64_t ]


    let t : t typ =
      enum "CXSaveTranslationUnit_Flags" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXSaveError = struct
    type t =
      | CXSaveError_None
      | CXSaveError_Unknown
      | CXSaveError_TranslationErrors
      | CXSaveError_InvalidTU

    let mapping =
      [ CXSaveError_None, constant "CXSaveError_None" int64_t
      ; CXSaveError_Unknown, constant "CXSaveError_Unknown" int64_t
      ; CXSaveError_TranslationErrors, constant "CXSaveError_TranslationErrors" int64_t
      ; CXSaveError_InvalidTU, constant "CXSaveError_InvalidTU" int64_t
      ]


    let t : t typ =
      enum "CXSaveError" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXReparse_Flags = struct
    type t = CXReparse_None

    let mapping = [ CXReparse_None, constant "CXReparse_None" int64_t ]

    let t : t typ =
      enum "CXReparse_Flags" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXTUResourceUsageKind = struct
    type t =
      | CXTUResourceUsage_AST
      | CXTUResourceUsage_Identifiers
      | CXTUResourceUsage_Selectors
      | CXTUResourceUsage_GlobalCompletionResults
      | CXTUResourceUsage_SourceManagerContentCache
      | CXTUResourceUsage_AST_SideTables
      | CXTUResourceUsage_SourceManager_Membuffer_Malloc
      | CXTUResourceUsage_SourceManager_Membuffer_MMap
      | CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc
      | CXTUResourceUsage_ExternalASTSource_Membuffer_MMap
      | CXTUResourceUsage_Preprocessor
      | CXTUResourceUsage_PreprocessingRecord
      | CXTUResourceUsage_SourceManager_DataStructures
      | CXTUResourceUsage_Preprocessor_HeaderSearch
      | CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN
      | CXTUResourceUsage_MEMORY_IN_BYTES_END
      | CXTUResourceUsage_First
      | CXTUResourceUsage_Last

    let mapping =
      [ CXTUResourceUsage_AST, constant "CXTUResourceUsage_AST" int64_t
      ; CXTUResourceUsage_Identifiers, constant "CXTUResourceUsage_Identifiers" int64_t
      ; CXTUResourceUsage_Selectors, constant "CXTUResourceUsage_Selectors" int64_t
      ; ( CXTUResourceUsage_GlobalCompletionResults
        , constant "CXTUResourceUsage_GlobalCompletionResults" int64_t )
      ; ( CXTUResourceUsage_SourceManagerContentCache
        , constant "CXTUResourceUsage_SourceManagerContentCache" int64_t )
      ; ( CXTUResourceUsage_AST_SideTables
        , constant "CXTUResourceUsage_AST_SideTables" int64_t )
      ; ( CXTUResourceUsage_SourceManager_Membuffer_Malloc
        , constant "CXTUResourceUsage_SourceManager_Membuffer_Malloc" int64_t )
      ; ( CXTUResourceUsage_SourceManager_Membuffer_MMap
        , constant "CXTUResourceUsage_SourceManager_Membuffer_MMap" int64_t )
      ; ( CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc
        , constant "CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc" int64_t )
      ; ( CXTUResourceUsage_ExternalASTSource_Membuffer_MMap
        , constant "CXTUResourceUsage_ExternalASTSource_Membuffer_MMap" int64_t )
      ; CXTUResourceUsage_Preprocessor, constant "CXTUResourceUsage_Preprocessor" int64_t
      ; ( CXTUResourceUsage_PreprocessingRecord
        , constant "CXTUResourceUsage_PreprocessingRecord" int64_t )
      ; ( CXTUResourceUsage_SourceManager_DataStructures
        , constant "CXTUResourceUsage_SourceManager_DataStructures" int64_t )
      ; ( CXTUResourceUsage_Preprocessor_HeaderSearch
        , constant "CXTUResourceUsage_Preprocessor_HeaderSearch" int64_t )
      ; ( CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN
        , constant "CXTUResourceUsage_MEMORY_IN_BYTES_BEGIN" int64_t )
      ; ( CXTUResourceUsage_MEMORY_IN_BYTES_END
        , constant "CXTUResourceUsage_MEMORY_IN_BYTES_END" int64_t )
      ; CXTUResourceUsage_First, constant "CXTUResourceUsage_First" int64_t
      ; CXTUResourceUsage_Last, constant "CXTUResourceUsage_Last" int64_t
      ]


    let t : t typ =
      enum "CXTUResourceUsageKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXTUResourceUsageEntry = struct
    type t

    let t : t structure typ =
      typedef (structure "CXTUResourceUsageEntry") "CXTUResourceUsageEntry"


    let kind = field t "kind" CXTUResourceUsageKind.t
    let amount = field t "amount" ulong
    let () = seal t
  end

  module CXTUResourceUsage = struct
    type t

    let t : t structure typ = typedef (structure "CXTUResourceUsage") "CXTUResourceUsage"
    let data = field t "data" (ptr void)
    let num_entries = field t "numEntries" uint
    let entries = field t "entries" (ptr CXTUResourceUsageEntry.t)
    let () = seal t
  end

  module CXCursorKind = struct
    type t =
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

    let mapping =
      [ CXCursor_UnexposedDecl, constant "CXCursor_UnexposedDecl" int64_t
      ; CXCursor_StructDecl, constant "CXCursor_StructDecl" int64_t
      ; CXCursor_UnionDecl, constant "CXCursor_UnionDecl" int64_t
      ; CXCursor_ClassDecl, constant "CXCursor_ClassDecl" int64_t
      ; CXCursor_EnumDecl, constant "CXCursor_EnumDecl" int64_t
      ; CXCursor_FieldDecl, constant "CXCursor_FieldDecl" int64_t
      ; CXCursor_EnumConstantDecl, constant "CXCursor_EnumConstantDecl" int64_t
      ; CXCursor_FunctionDecl, constant "CXCursor_FunctionDecl" int64_t
      ; CXCursor_VarDecl, constant "CXCursor_VarDecl" int64_t
      ; CXCursor_ParmDecl, constant "CXCursor_ParmDecl" int64_t
      ; CXCursor_ObjCInterfaceDecl, constant "CXCursor_ObjCInterfaceDecl" int64_t
      ; CXCursor_ObjCCategoryDecl, constant "CXCursor_ObjCCategoryDecl" int64_t
      ; CXCursor_ObjCProtocolDecl, constant "CXCursor_ObjCProtocolDecl" int64_t
      ; CXCursor_ObjCPropertyDecl, constant "CXCursor_ObjCPropertyDecl" int64_t
      ; CXCursor_ObjCIvarDecl, constant "CXCursor_ObjCIvarDecl" int64_t
      ; ( CXCursor_ObjCInstanceMethodDecl
        , constant "CXCursor_ObjCInstanceMethodDecl" int64_t )
      ; CXCursor_ObjCClassMethodDecl, constant "CXCursor_ObjCClassMethodDecl" int64_t
      ; ( CXCursor_ObjCImplementationDecl
        , constant "CXCursor_ObjCImplementationDecl" int64_t )
      ; CXCursor_ObjCCategoryImplDecl, constant "CXCursor_ObjCCategoryImplDecl" int64_t
      ; CXCursor_TypedefDecl, constant "CXCursor_TypedefDecl" int64_t
      ; CXCursor_CXXMethod, constant "CXCursor_CXXMethod" int64_t
      ; CXCursor_Namespace, constant "CXCursor_Namespace" int64_t
      ; CXCursor_LinkageSpec, constant "CXCursor_LinkageSpec" int64_t
      ; CXCursor_Constructor, constant "CXCursor_Constructor" int64_t
      ; CXCursor_Destructor, constant "CXCursor_Destructor" int64_t
      ; CXCursor_ConversionFunction, constant "CXCursor_ConversionFunction" int64_t
      ; CXCursor_TemplateTypeParameter, constant "CXCursor_TemplateTypeParameter" int64_t
      ; ( CXCursor_NonTypeTemplateParameter
        , constant "CXCursor_NonTypeTemplateParameter" int64_t )
      ; ( CXCursor_TemplateTemplateParameter
        , constant "CXCursor_TemplateTemplateParameter" int64_t )
      ; CXCursor_FunctionTemplate, constant "CXCursor_FunctionTemplate" int64_t
      ; CXCursor_ClassTemplate, constant "CXCursor_ClassTemplate" int64_t
      ; ( CXCursor_ClassTemplatePartialSpecialization
        , constant "CXCursor_ClassTemplatePartialSpecialization" int64_t )
      ; CXCursor_NamespaceAlias, constant "CXCursor_NamespaceAlias" int64_t
      ; CXCursor_UsingDirective, constant "CXCursor_UsingDirective" int64_t
      ; CXCursor_UsingDeclaration, constant "CXCursor_UsingDeclaration" int64_t
      ; CXCursor_TypeAliasDecl, constant "CXCursor_TypeAliasDecl" int64_t
      ; CXCursor_ObjCSynthesizeDecl, constant "CXCursor_ObjCSynthesizeDecl" int64_t
      ; CXCursor_ObjCDynamicDecl, constant "CXCursor_ObjCDynamicDecl" int64_t
      ; CXCursor_CXXAccessSpecifier, constant "CXCursor_CXXAccessSpecifier" int64_t
      ; CXCursor_FirstDecl, constant "CXCursor_FirstDecl" int64_t
      ; CXCursor_LastDecl, constant "CXCursor_LastDecl" int64_t
      ; CXCursor_FirstRef, constant "CXCursor_FirstRef" int64_t
      ; CXCursor_ObjCSuperClassRef, constant "CXCursor_ObjCSuperClassRef" int64_t
      ; CXCursor_ObjCProtocolRef, constant "CXCursor_ObjCProtocolRef" int64_t
      ; CXCursor_ObjCClassRef, constant "CXCursor_ObjCClassRef" int64_t
      ; CXCursor_TypeRef, constant "CXCursor_TypeRef" int64_t
      ; CXCursor_CXXBaseSpecifier, constant "CXCursor_CXXBaseSpecifier" int64_t
      ; CXCursor_TemplateRef, constant "CXCursor_TemplateRef" int64_t
      ; CXCursor_NamespaceRef, constant "CXCursor_NamespaceRef" int64_t
      ; CXCursor_MemberRef, constant "CXCursor_MemberRef" int64_t
      ; CXCursor_LabelRef, constant "CXCursor_LabelRef" int64_t
      ; CXCursor_OverloadedDeclRef, constant "CXCursor_OverloadedDeclRef" int64_t
      ; CXCursor_VariableRef, constant "CXCursor_VariableRef" int64_t
      ; CXCursor_LastRef, constant "CXCursor_LastRef" int64_t
      ; CXCursor_FirstInvalid, constant "CXCursor_FirstInvalid" int64_t
      ; CXCursor_InvalidFile, constant "CXCursor_InvalidFile" int64_t
      ; CXCursor_NoDeclFound, constant "CXCursor_NoDeclFound" int64_t
      ; CXCursor_NotImplemented, constant "CXCursor_NotImplemented" int64_t
      ; CXCursor_InvalidCode, constant "CXCursor_InvalidCode" int64_t
      ; CXCursor_LastInvalid, constant "CXCursor_LastInvalid" int64_t
      ; CXCursor_FirstExpr, constant "CXCursor_FirstExpr" int64_t
      ; CXCursor_UnexposedExpr, constant "CXCursor_UnexposedExpr" int64_t
      ; CXCursor_DeclRefExpr, constant "CXCursor_DeclRefExpr" int64_t
      ; CXCursor_MemberRefExpr, constant "CXCursor_MemberRefExpr" int64_t
      ; CXCursor_CallExpr, constant "CXCursor_CallExpr" int64_t
      ; CXCursor_ObjCMessageExpr, constant "CXCursor_ObjCMessageExpr" int64_t
      ; CXCursor_BlockExpr, constant "CXCursor_BlockExpr" int64_t
      ; CXCursor_IntegerLiteral, constant "CXCursor_IntegerLiteral" int64_t
      ; CXCursor_FloatingLiteral, constant "CXCursor_FloatingLiteral" int64_t
      ; CXCursor_ImaginaryLiteral, constant "CXCursor_ImaginaryLiteral" int64_t
      ; CXCursor_StringLiteral, constant "CXCursor_StringLiteral" int64_t
      ; CXCursor_CharacterLiteral, constant "CXCursor_CharacterLiteral" int64_t
      ; CXCursor_ParenExpr, constant "CXCursor_ParenExpr" int64_t
      ; CXCursor_UnaryOperator, constant "CXCursor_UnaryOperator" int64_t
      ; CXCursor_ArraySubscriptExpr, constant "CXCursor_ArraySubscriptExpr" int64_t
      ; CXCursor_BinaryOperator, constant "CXCursor_BinaryOperator" int64_t
      ; ( CXCursor_CompoundAssignOperator
        , constant "CXCursor_CompoundAssignOperator" int64_t )
      ; CXCursor_ConditionalOperator, constant "CXCursor_ConditionalOperator" int64_t
      ; CXCursor_CStyleCastExpr, constant "CXCursor_CStyleCastExpr" int64_t
      ; CXCursor_CompoundLiteralExpr, constant "CXCursor_CompoundLiteralExpr" int64_t
      ; CXCursor_InitListExpr, constant "CXCursor_InitListExpr" int64_t
      ; CXCursor_AddrLabelExpr, constant "CXCursor_AddrLabelExpr" int64_t
      ; CXCursor_StmtExpr, constant "CXCursor_StmtExpr" int64_t
      ; CXCursor_GenericSelectionExpr, constant "CXCursor_GenericSelectionExpr" int64_t
      ; CXCursor_GNUNullExpr, constant "CXCursor_GNUNullExpr" int64_t
      ; CXCursor_CXXStaticCastExpr, constant "CXCursor_CXXStaticCastExpr" int64_t
      ; CXCursor_CXXDynamicCastExpr, constant "CXCursor_CXXDynamicCastExpr" int64_t
      ; ( CXCursor_CXXReinterpretCastExpr
        , constant "CXCursor_CXXReinterpretCastExpr" int64_t )
      ; CXCursor_CXXConstCastExpr, constant "CXCursor_CXXConstCastExpr" int64_t
      ; CXCursor_CXXFunctionalCastExpr, constant "CXCursor_CXXFunctionalCastExpr" int64_t
      ; CXCursor_CXXTypeidExpr, constant "CXCursor_CXXTypeidExpr" int64_t
      ; CXCursor_CXXBoolLiteralExpr, constant "CXCursor_CXXBoolLiteralExpr" int64_t
      ; CXCursor_CXXNullPtrLiteralExpr, constant "CXCursor_CXXNullPtrLiteralExpr" int64_t
      ; CXCursor_CXXThisExpr, constant "CXCursor_CXXThisExpr" int64_t
      ; CXCursor_CXXThrowExpr, constant "CXCursor_CXXThrowExpr" int64_t
      ; CXCursor_CXXNewExpr, constant "CXCursor_CXXNewExpr" int64_t
      ; CXCursor_CXXDeleteExpr, constant "CXCursor_CXXDeleteExpr" int64_t
      ; CXCursor_UnaryExpr, constant "CXCursor_UnaryExpr" int64_t
      ; CXCursor_ObjCStringLiteral, constant "CXCursor_ObjCStringLiteral" int64_t
      ; CXCursor_ObjCEncodeExpr, constant "CXCursor_ObjCEncodeExpr" int64_t
      ; CXCursor_ObjCSelectorExpr, constant "CXCursor_ObjCSelectorExpr" int64_t
      ; CXCursor_ObjCProtocolExpr, constant "CXCursor_ObjCProtocolExpr" int64_t
      ; CXCursor_ObjCBridgedCastExpr, constant "CXCursor_ObjCBridgedCastExpr" int64_t
      ; CXCursor_PackExpansionExpr, constant "CXCursor_PackExpansionExpr" int64_t
      ; CXCursor_SizeOfPackExpr, constant "CXCursor_SizeOfPackExpr" int64_t
      ; CXCursor_LambdaExpr, constant "CXCursor_LambdaExpr" int64_t
      ; CXCursor_ObjCBoolLiteralExpr, constant "CXCursor_ObjCBoolLiteralExpr" int64_t
      ; CXCursor_ObjCSelfExpr, constant "CXCursor_ObjCSelfExpr" int64_t
      ; CXCursor_ArraySectionExpr, constant "CXCursor_ArraySectionExpr" int64_t
      ; ( CXCursor_ObjCAvailabilityCheckExpr
        , constant "CXCursor_ObjCAvailabilityCheckExpr" int64_t )
      ; CXCursor_FixedPointLiteral, constant "CXCursor_FixedPointLiteral" int64_t
      ; CXCursor_OMPArrayShapingExpr, constant "CXCursor_OMPArrayShapingExpr" int64_t
      ; CXCursor_OMPIteratorExpr, constant "CXCursor_OMPIteratorExpr" int64_t
      ; CXCursor_CXXAddrspaceCastExpr, constant "CXCursor_CXXAddrspaceCastExpr" int64_t
      ; ( CXCursor_ConceptSpecializationExpr
        , constant "CXCursor_ConceptSpecializationExpr" int64_t )
      ; CXCursor_RequiresExpr, constant "CXCursor_RequiresExpr" int64_t
      ; CXCursor_CXXParenListInitExpr, constant "CXCursor_CXXParenListInitExpr" int64_t
      ; CXCursor_PackIndexingExpr, constant "CXCursor_PackIndexingExpr" int64_t
      ; CXCursor_LastExpr, constant "CXCursor_LastExpr" int64_t
      ; CXCursor_FirstStmt, constant "CXCursor_FirstStmt" int64_t
      ; CXCursor_UnexposedStmt, constant "CXCursor_UnexposedStmt" int64_t
      ; CXCursor_LabelStmt, constant "CXCursor_LabelStmt" int64_t
      ; CXCursor_CompoundStmt, constant "CXCursor_CompoundStmt" int64_t
      ; CXCursor_CaseStmt, constant "CXCursor_CaseStmt" int64_t
      ; CXCursor_DefaultStmt, constant "CXCursor_DefaultStmt" int64_t
      ; CXCursor_IfStmt, constant "CXCursor_IfStmt" int64_t
      ; CXCursor_SwitchStmt, constant "CXCursor_SwitchStmt" int64_t
      ; CXCursor_WhileStmt, constant "CXCursor_WhileStmt" int64_t
      ; CXCursor_DoStmt, constant "CXCursor_DoStmt" int64_t
      ; CXCursor_ForStmt, constant "CXCursor_ForStmt" int64_t
      ; CXCursor_GotoStmt, constant "CXCursor_GotoStmt" int64_t
      ; CXCursor_IndirectGotoStmt, constant "CXCursor_IndirectGotoStmt" int64_t
      ; CXCursor_ContinueStmt, constant "CXCursor_ContinueStmt" int64_t
      ; CXCursor_BreakStmt, constant "CXCursor_BreakStmt" int64_t
      ; CXCursor_ReturnStmt, constant "CXCursor_ReturnStmt" int64_t
      ; CXCursor_GCCAsmStmt, constant "CXCursor_GCCAsmStmt" int64_t
      ; CXCursor_AsmStmt, constant "CXCursor_AsmStmt" int64_t
      ; CXCursor_ObjCAtTryStmt, constant "CXCursor_ObjCAtTryStmt" int64_t
      ; CXCursor_ObjCAtCatchStmt, constant "CXCursor_ObjCAtCatchStmt" int64_t
      ; CXCursor_ObjCAtFinallyStmt, constant "CXCursor_ObjCAtFinallyStmt" int64_t
      ; CXCursor_ObjCAtThrowStmt, constant "CXCursor_ObjCAtThrowStmt" int64_t
      ; ( CXCursor_ObjCAtSynchronizedStmt
        , constant "CXCursor_ObjCAtSynchronizedStmt" int64_t )
      ; ( CXCursor_ObjCAutoreleasePoolStmt
        , constant "CXCursor_ObjCAutoreleasePoolStmt" int64_t )
      ; CXCursor_ObjCForCollectionStmt, constant "CXCursor_ObjCForCollectionStmt" int64_t
      ; CXCursor_CXXCatchStmt, constant "CXCursor_CXXCatchStmt" int64_t
      ; CXCursor_CXXTryStmt, constant "CXCursor_CXXTryStmt" int64_t
      ; CXCursor_CXXForRangeStmt, constant "CXCursor_CXXForRangeStmt" int64_t
      ; CXCursor_SEHTryStmt, constant "CXCursor_SEHTryStmt" int64_t
      ; CXCursor_SEHExceptStmt, constant "CXCursor_SEHExceptStmt" int64_t
      ; CXCursor_SEHFinallyStmt, constant "CXCursor_SEHFinallyStmt" int64_t
      ; CXCursor_MSAsmStmt, constant "CXCursor_MSAsmStmt" int64_t
      ; CXCursor_NullStmt, constant "CXCursor_NullStmt" int64_t
      ; CXCursor_DeclStmt, constant "CXCursor_DeclStmt" int64_t
      ; CXCursor_OMPParallelDirective, constant "CXCursor_OMPParallelDirective" int64_t
      ; CXCursor_OMPSimdDirective, constant "CXCursor_OMPSimdDirective" int64_t
      ; CXCursor_OMPForDirective, constant "CXCursor_OMPForDirective" int64_t
      ; CXCursor_OMPSectionsDirective, constant "CXCursor_OMPSectionsDirective" int64_t
      ; CXCursor_OMPSectionDirective, constant "CXCursor_OMPSectionDirective" int64_t
      ; CXCursor_OMPSingleDirective, constant "CXCursor_OMPSingleDirective" int64_t
      ; ( CXCursor_OMPParallelForDirective
        , constant "CXCursor_OMPParallelForDirective" int64_t )
      ; ( CXCursor_OMPParallelSectionsDirective
        , constant "CXCursor_OMPParallelSectionsDirective" int64_t )
      ; CXCursor_OMPTaskDirective, constant "CXCursor_OMPTaskDirective" int64_t
      ; CXCursor_OMPMasterDirective, constant "CXCursor_OMPMasterDirective" int64_t
      ; CXCursor_OMPCriticalDirective, constant "CXCursor_OMPCriticalDirective" int64_t
      ; CXCursor_OMPTaskyieldDirective, constant "CXCursor_OMPTaskyieldDirective" int64_t
      ; CXCursor_OMPBarrierDirective, constant "CXCursor_OMPBarrierDirective" int64_t
      ; CXCursor_OMPTaskwaitDirective, constant "CXCursor_OMPTaskwaitDirective" int64_t
      ; CXCursor_OMPFlushDirective, constant "CXCursor_OMPFlushDirective" int64_t
      ; CXCursor_SEHLeaveStmt, constant "CXCursor_SEHLeaveStmt" int64_t
      ; CXCursor_OMPOrderedDirective, constant "CXCursor_OMPOrderedDirective" int64_t
      ; CXCursor_OMPAtomicDirective, constant "CXCursor_OMPAtomicDirective" int64_t
      ; CXCursor_OMPForSimdDirective, constant "CXCursor_OMPForSimdDirective" int64_t
      ; ( CXCursor_OMPParallelForSimdDirective
        , constant "CXCursor_OMPParallelForSimdDirective" int64_t )
      ; CXCursor_OMPTargetDirective, constant "CXCursor_OMPTargetDirective" int64_t
      ; CXCursor_OMPTeamsDirective, constant "CXCursor_OMPTeamsDirective" int64_t
      ; CXCursor_OMPTaskgroupDirective, constant "CXCursor_OMPTaskgroupDirective" int64_t
      ; ( CXCursor_OMPCancellationPointDirective
        , constant "CXCursor_OMPCancellationPointDirective" int64_t )
      ; CXCursor_OMPCancelDirective, constant "CXCursor_OMPCancelDirective" int64_t
      ; ( CXCursor_OMPTargetDataDirective
        , constant "CXCursor_OMPTargetDataDirective" int64_t )
      ; CXCursor_OMPTaskLoopDirective, constant "CXCursor_OMPTaskLoopDirective" int64_t
      ; ( CXCursor_OMPTaskLoopSimdDirective
        , constant "CXCursor_OMPTaskLoopSimdDirective" int64_t )
      ; ( CXCursor_OMPDistributeDirective
        , constant "CXCursor_OMPDistributeDirective" int64_t )
      ; ( CXCursor_OMPTargetEnterDataDirective
        , constant "CXCursor_OMPTargetEnterDataDirective" int64_t )
      ; ( CXCursor_OMPTargetExitDataDirective
        , constant "CXCursor_OMPTargetExitDataDirective" int64_t )
      ; ( CXCursor_OMPTargetParallelDirective
        , constant "CXCursor_OMPTargetParallelDirective" int64_t )
      ; ( CXCursor_OMPTargetParallelForDirective
        , constant "CXCursor_OMPTargetParallelForDirective" int64_t )
      ; ( CXCursor_OMPTargetUpdateDirective
        , constant "CXCursor_OMPTargetUpdateDirective" int64_t )
      ; ( CXCursor_OMPDistributeParallelForDirective
        , constant "CXCursor_OMPDistributeParallelForDirective" int64_t )
      ; ( CXCursor_OMPDistributeParallelForSimdDirective
        , constant "CXCursor_OMPDistributeParallelForSimdDirective" int64_t )
      ; ( CXCursor_OMPDistributeSimdDirective
        , constant "CXCursor_OMPDistributeSimdDirective" int64_t )
      ; ( CXCursor_OMPTargetParallelForSimdDirective
        , constant "CXCursor_OMPTargetParallelForSimdDirective" int64_t )
      ; ( CXCursor_OMPTargetSimdDirective
        , constant "CXCursor_OMPTargetSimdDirective" int64_t )
      ; ( CXCursor_OMPTeamsDistributeDirective
        , constant "CXCursor_OMPTeamsDistributeDirective" int64_t )
      ; ( CXCursor_OMPTeamsDistributeSimdDirective
        , constant "CXCursor_OMPTeamsDistributeSimdDirective" int64_t )
      ; ( CXCursor_OMPTeamsDistributeParallelForSimdDirective
        , constant "CXCursor_OMPTeamsDistributeParallelForSimdDirective" int64_t )
      ; ( CXCursor_OMPTeamsDistributeParallelForDirective
        , constant "CXCursor_OMPTeamsDistributeParallelForDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsDirective
        , constant "CXCursor_OMPTargetTeamsDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsDistributeDirective
        , constant "CXCursor_OMPTargetTeamsDistributeDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsDistributeParallelForDirective
        , constant "CXCursor_OMPTargetTeamsDistributeParallelForDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective
        , constant "CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsDistributeSimdDirective
        , constant "CXCursor_OMPTargetTeamsDistributeSimdDirective" int64_t )
      ; CXCursor_BuiltinBitCastExpr, constant "CXCursor_BuiltinBitCastExpr" int64_t
      ; ( CXCursor_OMPMasterTaskLoopDirective
        , constant "CXCursor_OMPMasterTaskLoopDirective" int64_t )
      ; ( CXCursor_OMPParallelMasterTaskLoopDirective
        , constant "CXCursor_OMPParallelMasterTaskLoopDirective" int64_t )
      ; ( CXCursor_OMPMasterTaskLoopSimdDirective
        , constant "CXCursor_OMPMasterTaskLoopSimdDirective" int64_t )
      ; ( CXCursor_OMPParallelMasterTaskLoopSimdDirective
        , constant "CXCursor_OMPParallelMasterTaskLoopSimdDirective" int64_t )
      ; ( CXCursor_OMPParallelMasterDirective
        , constant "CXCursor_OMPParallelMasterDirective" int64_t )
      ; CXCursor_OMPDepobjDirective, constant "CXCursor_OMPDepobjDirective" int64_t
      ; CXCursor_OMPScanDirective, constant "CXCursor_OMPScanDirective" int64_t
      ; CXCursor_OMPTileDirective, constant "CXCursor_OMPTileDirective" int64_t
      ; CXCursor_OMPCanonicalLoop, constant "CXCursor_OMPCanonicalLoop" int64_t
      ; CXCursor_OMPInteropDirective, constant "CXCursor_OMPInteropDirective" int64_t
      ; CXCursor_OMPDispatchDirective, constant "CXCursor_OMPDispatchDirective" int64_t
      ; CXCursor_OMPMaskedDirective, constant "CXCursor_OMPMaskedDirective" int64_t
      ; CXCursor_OMPUnrollDirective, constant "CXCursor_OMPUnrollDirective" int64_t
      ; CXCursor_OMPMetaDirective, constant "CXCursor_OMPMetaDirective" int64_t
      ; ( CXCursor_OMPGenericLoopDirective
        , constant "CXCursor_OMPGenericLoopDirective" int64_t )
      ; ( CXCursor_OMPTeamsGenericLoopDirective
        , constant "CXCursor_OMPTeamsGenericLoopDirective" int64_t )
      ; ( CXCursor_OMPTargetTeamsGenericLoopDirective
        , constant "CXCursor_OMPTargetTeamsGenericLoopDirective" int64_t )
      ; ( CXCursor_OMPParallelGenericLoopDirective
        , constant "CXCursor_OMPParallelGenericLoopDirective" int64_t )
      ; ( CXCursor_OMPTargetParallelGenericLoopDirective
        , constant "CXCursor_OMPTargetParallelGenericLoopDirective" int64_t )
      ; ( CXCursor_OMPParallelMaskedDirective
        , constant "CXCursor_OMPParallelMaskedDirective" int64_t )
      ; ( CXCursor_OMPMaskedTaskLoopDirective
        , constant "CXCursor_OMPMaskedTaskLoopDirective" int64_t )
      ; ( CXCursor_OMPMaskedTaskLoopSimdDirective
        , constant "CXCursor_OMPMaskedTaskLoopSimdDirective" int64_t )
      ; ( CXCursor_OMPParallelMaskedTaskLoopDirective
        , constant "CXCursor_OMPParallelMaskedTaskLoopDirective" int64_t )
      ; ( CXCursor_OMPParallelMaskedTaskLoopSimdDirective
        , constant "CXCursor_OMPParallelMaskedTaskLoopSimdDirective" int64_t )
      ; CXCursor_OMPErrorDirective, constant "CXCursor_OMPErrorDirective" int64_t
      ; CXCursor_OMPScopeDirective, constant "CXCursor_OMPScopeDirective" int64_t
      ; CXCursor_OMPReverseDirective, constant "CXCursor_OMPReverseDirective" int64_t
      ; ( CXCursor_OMPInterchangeDirective
        , constant "CXCursor_OMPInterchangeDirective" int64_t )
      ; ( CXCursor_OpenACCComputeConstruct
        , constant "CXCursor_OpenACCComputeConstruct" int64_t )
      ; CXCursor_OpenACCLoopConstruct, constant "CXCursor_OpenACCLoopConstruct" int64_t
      ; CXCursor_LastStmt, constant "CXCursor_LastStmt" int64_t
      ; CXCursor_TranslationUnit, constant "CXCursor_TranslationUnit" int64_t
      ; CXCursor_FirstAttr, constant "CXCursor_FirstAttr" int64_t
      ; CXCursor_UnexposedAttr, constant "CXCursor_UnexposedAttr" int64_t
      ; CXCursor_IBActionAttr, constant "CXCursor_IBActionAttr" int64_t
      ; CXCursor_IBOutletAttr, constant "CXCursor_IBOutletAttr" int64_t
      ; ( CXCursor_IBOutletCollectionAttr
        , constant "CXCursor_IBOutletCollectionAttr" int64_t )
      ; CXCursor_CXXFinalAttr, constant "CXCursor_CXXFinalAttr" int64_t
      ; CXCursor_CXXOverrideAttr, constant "CXCursor_CXXOverrideAttr" int64_t
      ; CXCursor_AnnotateAttr, constant "CXCursor_AnnotateAttr" int64_t
      ; CXCursor_AsmLabelAttr, constant "CXCursor_AsmLabelAttr" int64_t
      ; CXCursor_PackedAttr, constant "CXCursor_PackedAttr" int64_t
      ; CXCursor_PureAttr, constant "CXCursor_PureAttr" int64_t
      ; CXCursor_ConstAttr, constant "CXCursor_ConstAttr" int64_t
      ; CXCursor_NoDuplicateAttr, constant "CXCursor_NoDuplicateAttr" int64_t
      ; CXCursor_CUDAConstantAttr, constant "CXCursor_CUDAConstantAttr" int64_t
      ; CXCursor_CUDADeviceAttr, constant "CXCursor_CUDADeviceAttr" int64_t
      ; CXCursor_CUDAGlobalAttr, constant "CXCursor_CUDAGlobalAttr" int64_t
      ; CXCursor_CUDAHostAttr, constant "CXCursor_CUDAHostAttr" int64_t
      ; CXCursor_CUDASharedAttr, constant "CXCursor_CUDASharedAttr" int64_t
      ; CXCursor_VisibilityAttr, constant "CXCursor_VisibilityAttr" int64_t
      ; CXCursor_DLLExport, constant "CXCursor_DLLExport" int64_t
      ; CXCursor_DLLImport, constant "CXCursor_DLLImport" int64_t
      ; CXCursor_NSReturnsRetained, constant "CXCursor_NSReturnsRetained" int64_t
      ; CXCursor_NSReturnsNotRetained, constant "CXCursor_NSReturnsNotRetained" int64_t
      ; CXCursor_NSReturnsAutoreleased, constant "CXCursor_NSReturnsAutoreleased" int64_t
      ; CXCursor_NSConsumesSelf, constant "CXCursor_NSConsumesSelf" int64_t
      ; CXCursor_NSConsumed, constant "CXCursor_NSConsumed" int64_t
      ; CXCursor_ObjCException, constant "CXCursor_ObjCException" int64_t
      ; CXCursor_ObjCNSObject, constant "CXCursor_ObjCNSObject" int64_t
      ; CXCursor_ObjCIndependentClass, constant "CXCursor_ObjCIndependentClass" int64_t
      ; CXCursor_ObjCPreciseLifetime, constant "CXCursor_ObjCPreciseLifetime" int64_t
      ; ( CXCursor_ObjCReturnsInnerPointer
        , constant "CXCursor_ObjCReturnsInnerPointer" int64_t )
      ; CXCursor_ObjCRequiresSuper, constant "CXCursor_ObjCRequiresSuper" int64_t
      ; CXCursor_ObjCRootClass, constant "CXCursor_ObjCRootClass" int64_t
      ; ( CXCursor_ObjCSubclassingRestricted
        , constant "CXCursor_ObjCSubclassingRestricted" int64_t )
      ; ( CXCursor_ObjCExplicitProtocolImpl
        , constant "CXCursor_ObjCExplicitProtocolImpl" int64_t )
      ; ( CXCursor_ObjCDesignatedInitializer
        , constant "CXCursor_ObjCDesignatedInitializer" int64_t )
      ; CXCursor_ObjCRuntimeVisible, constant "CXCursor_ObjCRuntimeVisible" int64_t
      ; CXCursor_ObjCBoxable, constant "CXCursor_ObjCBoxable" int64_t
      ; CXCursor_FlagEnum, constant "CXCursor_FlagEnum" int64_t
      ; CXCursor_ConvergentAttr, constant "CXCursor_ConvergentAttr" int64_t
      ; CXCursor_WarnUnusedAttr, constant "CXCursor_WarnUnusedAttr" int64_t
      ; CXCursor_WarnUnusedResultAttr, constant "CXCursor_WarnUnusedResultAttr" int64_t
      ; CXCursor_AlignedAttr, constant "CXCursor_AlignedAttr" int64_t
      ; CXCursor_LastAttr, constant "CXCursor_LastAttr" int64_t
      ; ( CXCursor_PreprocessingDirective
        , constant "CXCursor_PreprocessingDirective" int64_t )
      ; CXCursor_MacroDefinition, constant "CXCursor_MacroDefinition" int64_t
      ; CXCursor_MacroExpansion, constant "CXCursor_MacroExpansion" int64_t
      ; CXCursor_MacroInstantiation, constant "CXCursor_MacroInstantiation" int64_t
      ; CXCursor_InclusionDirective, constant "CXCursor_InclusionDirective" int64_t
      ; CXCursor_FirstPreprocessing, constant "CXCursor_FirstPreprocessing" int64_t
      ; CXCursor_LastPreprocessing, constant "CXCursor_LastPreprocessing" int64_t
      ; CXCursor_ModuleImportDecl, constant "CXCursor_ModuleImportDecl" int64_t
      ; CXCursor_TypeAliasTemplateDecl, constant "CXCursor_TypeAliasTemplateDecl" int64_t
      ; CXCursor_StaticAssert, constant "CXCursor_StaticAssert" int64_t
      ; CXCursor_FriendDecl, constant "CXCursor_FriendDecl" int64_t
      ; CXCursor_ConceptDecl, constant "CXCursor_ConceptDecl" int64_t
      ; CXCursor_FirstExtraDecl, constant "CXCursor_FirstExtraDecl" int64_t
      ; CXCursor_LastExtraDecl, constant "CXCursor_LastExtraDecl" int64_t
      ; CXCursor_OverloadCandidate, constant "CXCursor_OverloadCandidate" int64_t
      ]


    let t : t typ =
      enum "CXCursorKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXCursor = struct
    type t

    let t : t structure typ = typedef (structure "CXCursor") "CXCursor"
    let kind = field t "kind" int64_t
    let xdata = field t "xdata" int
    let data = field t "data" (ptr (ptr (const void)))
    let () = seal t
  end

  module CXLinkageKind = struct
    type t =
      | CXLinkage_Invalid
      | CXLinkage_NoLinkage
      | CXLinkage_Internal
      | CXLinkage_UniqueExternal
      | CXLinkage_External

    let mapping =
      [ CXLinkage_Invalid, constant "CXLinkage_Invalid" int64_t
      ; CXLinkage_NoLinkage, constant "CXLinkage_NoLinkage" int64_t
      ; CXLinkage_Internal, constant "CXLinkage_Internal" int64_t
      ; CXLinkage_UniqueExternal, constant "CXLinkage_UniqueExternal" int64_t
      ; CXLinkage_External, constant "CXLinkage_External" int64_t
      ]


    let t : t typ =
      enum "CXLinkageKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXVisibilityKind = struct
    type t =
      | CXVisibility_Invalid
      | CXVisibility_Hidden
      | CXVisibility_Protected
      | CXVisibility_Default

    let mapping =
      [ CXVisibility_Invalid, constant "CXVisibility_Invalid" int64_t
      ; CXVisibility_Hidden, constant "CXVisibility_Hidden" int64_t
      ; CXVisibility_Protected, constant "CXVisibility_Protected" int64_t
      ; CXVisibility_Default, constant "CXVisibility_Default" int64_t
      ]


    let t : t typ =
      enum "CXVisibilityKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXString = struct
    type t

    let t : t structure typ = typedef (structure "CXString") "CXString"
    let data = field t "data" (ptr (const void))
    let private_flags = field t "private_flags" uint
    let () = seal t
  end

  module CXPlatformAvailability = struct
    type t

    let t : t structure typ =
      typedef (structure "CXPlatformAvailability") "CXPlatformAvailability"


    let platform = field t "Platform" CXString.t
    let introduced = field t "Introduced" CXVersion.t
    let deprecated = field t "Deprecated" CXVersion.t
    let obsoleted = field t "Obsoleted" CXVersion.t
    let unavailable = field t "Unavailable" int
    let message = field t "Message" CXString.t
    let () = seal t
  end

  module CXLanguageKind = struct
    type t =
      | CXLanguage_Invalid
      | CXLanguage_C
      | CXLanguage_ObjC
      | CXLanguage_CPlusPlus

    let mapping =
      [ CXLanguage_Invalid, constant "CXLanguage_Invalid" int64_t
      ; CXLanguage_C, constant "CXLanguage_C" int64_t
      ; CXLanguage_ObjC, constant "CXLanguage_ObjC" int64_t
      ; CXLanguage_CPlusPlus, constant "CXLanguage_CPlusPlus" int64_t
      ]


    let t : t typ =
      enum "CXLanguageKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXTLSKind = struct
    type t =
      | CXTLS_None
      | CXTLS_Dynamic
      | CXTLS_Static

    let mapping =
      [ CXTLS_None, constant "CXTLS_None" int64_t
      ; CXTLS_Dynamic, constant "CXTLS_Dynamic" int64_t
      ; CXTLS_Static, constant "CXTLS_Static" int64_t
      ]


    let t : t typ =
      enum "CXTLSKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXCursorSet = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXCursorSet"
  end

  module CXTypeKind = struct
    type t =
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

    let mapping =
      [ CXType_Invalid, constant "CXType_Invalid" int64_t
      ; CXType_Unexposed, constant "CXType_Unexposed" int64_t
      ; CXType_Void, constant "CXType_Void" int64_t
      ; CXType_Bool, constant "CXType_Bool" int64_t
      ; CXType_Char_U, constant "CXType_Char_U" int64_t
      ; CXType_UChar, constant "CXType_UChar" int64_t
      ; CXType_Char16, constant "CXType_Char16" int64_t
      ; CXType_Char32, constant "CXType_Char32" int64_t
      ; CXType_UShort, constant "CXType_UShort" int64_t
      ; CXType_UInt, constant "CXType_UInt" int64_t
      ; CXType_ULong, constant "CXType_ULong" int64_t
      ; CXType_ULongLong, constant "CXType_ULongLong" int64_t
      ; CXType_UInt128, constant "CXType_UInt128" int64_t
      ; CXType_Char_S, constant "CXType_Char_S" int64_t
      ; CXType_SChar, constant "CXType_SChar" int64_t
      ; CXType_WChar, constant "CXType_WChar" int64_t
      ; CXType_Short, constant "CXType_Short" int64_t
      ; CXType_Int, constant "CXType_Int" int64_t
      ; CXType_Long, constant "CXType_Long" int64_t
      ; CXType_LongLong, constant "CXType_LongLong" int64_t
      ; CXType_Int128, constant "CXType_Int128" int64_t
      ; CXType_Float, constant "CXType_Float" int64_t
      ; CXType_Double, constant "CXType_Double" int64_t
      ; CXType_LongDouble, constant "CXType_LongDouble" int64_t
      ; CXType_NullPtr, constant "CXType_NullPtr" int64_t
      ; CXType_Overload, constant "CXType_Overload" int64_t
      ; CXType_Dependent, constant "CXType_Dependent" int64_t
      ; CXType_ObjCId, constant "CXType_ObjCId" int64_t
      ; CXType_ObjCClass, constant "CXType_ObjCClass" int64_t
      ; CXType_ObjCSel, constant "CXType_ObjCSel" int64_t
      ; CXType_Float128, constant "CXType_Float128" int64_t
      ; CXType_Half, constant "CXType_Half" int64_t
      ; CXType_Float16, constant "CXType_Float16" int64_t
      ; CXType_ShortAccum, constant "CXType_ShortAccum" int64_t
      ; CXType_Accum, constant "CXType_Accum" int64_t
      ; CXType_LongAccum, constant "CXType_LongAccum" int64_t
      ; CXType_UShortAccum, constant "CXType_UShortAccum" int64_t
      ; CXType_UAccum, constant "CXType_UAccum" int64_t
      ; CXType_ULongAccum, constant "CXType_ULongAccum" int64_t
      ; CXType_BFloat16, constant "CXType_BFloat16" int64_t
      ; CXType_Ibm128, constant "CXType_Ibm128" int64_t
      ; CXType_FirstBuiltin, constant "CXType_FirstBuiltin" int64_t
      ; CXType_LastBuiltin, constant "CXType_LastBuiltin" int64_t
      ; CXType_Complex, constant "CXType_Complex" int64_t
      ; CXType_Pointer, constant "CXType_Pointer" int64_t
      ; CXType_BlockPointer, constant "CXType_BlockPointer" int64_t
      ; CXType_LValueReference, constant "CXType_LValueReference" int64_t
      ; CXType_RValueReference, constant "CXType_RValueReference" int64_t
      ; CXType_Record, constant "CXType_Record" int64_t
      ; CXType_Enum, constant "CXType_Enum" int64_t
      ; CXType_Typedef, constant "CXType_Typedef" int64_t
      ; CXType_ObjCInterface, constant "CXType_ObjCInterface" int64_t
      ; CXType_ObjCObjectPointer, constant "CXType_ObjCObjectPointer" int64_t
      ; CXType_FunctionNoProto, constant "CXType_FunctionNoProto" int64_t
      ; CXType_FunctionProto, constant "CXType_FunctionProto" int64_t
      ; CXType_ConstantArray, constant "CXType_ConstantArray" int64_t
      ; CXType_Vector, constant "CXType_Vector" int64_t
      ; CXType_IncompleteArray, constant "CXType_IncompleteArray" int64_t
      ; CXType_VariableArray, constant "CXType_VariableArray" int64_t
      ; CXType_DependentSizedArray, constant "CXType_DependentSizedArray" int64_t
      ; CXType_MemberPointer, constant "CXType_MemberPointer" int64_t
      ; CXType_Auto, constant "CXType_Auto" int64_t
      ; CXType_Elaborated, constant "CXType_Elaborated" int64_t
      ; CXType_Pipe, constant "CXType_Pipe" int64_t
      ; CXType_OCLImage1dRO, constant "CXType_OCLImage1dRO" int64_t
      ; CXType_OCLImage1dArrayRO, constant "CXType_OCLImage1dArrayRO" int64_t
      ; CXType_OCLImage1dBufferRO, constant "CXType_OCLImage1dBufferRO" int64_t
      ; CXType_OCLImage2dRO, constant "CXType_OCLImage2dRO" int64_t
      ; CXType_OCLImage2dArrayRO, constant "CXType_OCLImage2dArrayRO" int64_t
      ; CXType_OCLImage2dDepthRO, constant "CXType_OCLImage2dDepthRO" int64_t
      ; CXType_OCLImage2dArrayDepthRO, constant "CXType_OCLImage2dArrayDepthRO" int64_t
      ; CXType_OCLImage2dMSAARO, constant "CXType_OCLImage2dMSAARO" int64_t
      ; CXType_OCLImage2dArrayMSAARO, constant "CXType_OCLImage2dArrayMSAARO" int64_t
      ; CXType_OCLImage2dMSAADepthRO, constant "CXType_OCLImage2dMSAADepthRO" int64_t
      ; ( CXType_OCLImage2dArrayMSAADepthRO
        , constant "CXType_OCLImage2dArrayMSAADepthRO" int64_t )
      ; CXType_OCLImage3dRO, constant "CXType_OCLImage3dRO" int64_t
      ; CXType_OCLImage1dWO, constant "CXType_OCLImage1dWO" int64_t
      ; CXType_OCLImage1dArrayWO, constant "CXType_OCLImage1dArrayWO" int64_t
      ; CXType_OCLImage1dBufferWO, constant "CXType_OCLImage1dBufferWO" int64_t
      ; CXType_OCLImage2dWO, constant "CXType_OCLImage2dWO" int64_t
      ; CXType_OCLImage2dArrayWO, constant "CXType_OCLImage2dArrayWO" int64_t
      ; CXType_OCLImage2dDepthWO, constant "CXType_OCLImage2dDepthWO" int64_t
      ; CXType_OCLImage2dArrayDepthWO, constant "CXType_OCLImage2dArrayDepthWO" int64_t
      ; CXType_OCLImage2dMSAAWO, constant "CXType_OCLImage2dMSAAWO" int64_t
      ; CXType_OCLImage2dArrayMSAAWO, constant "CXType_OCLImage2dArrayMSAAWO" int64_t
      ; CXType_OCLImage2dMSAADepthWO, constant "CXType_OCLImage2dMSAADepthWO" int64_t
      ; ( CXType_OCLImage2dArrayMSAADepthWO
        , constant "CXType_OCLImage2dArrayMSAADepthWO" int64_t )
      ; CXType_OCLImage3dWO, constant "CXType_OCLImage3dWO" int64_t
      ; CXType_OCLImage1dRW, constant "CXType_OCLImage1dRW" int64_t
      ; CXType_OCLImage1dArrayRW, constant "CXType_OCLImage1dArrayRW" int64_t
      ; CXType_OCLImage1dBufferRW, constant "CXType_OCLImage1dBufferRW" int64_t
      ; CXType_OCLImage2dRW, constant "CXType_OCLImage2dRW" int64_t
      ; CXType_OCLImage2dArrayRW, constant "CXType_OCLImage2dArrayRW" int64_t
      ; CXType_OCLImage2dDepthRW, constant "CXType_OCLImage2dDepthRW" int64_t
      ; CXType_OCLImage2dArrayDepthRW, constant "CXType_OCLImage2dArrayDepthRW" int64_t
      ; CXType_OCLImage2dMSAARW, constant "CXType_OCLImage2dMSAARW" int64_t
      ; CXType_OCLImage2dArrayMSAARW, constant "CXType_OCLImage2dArrayMSAARW" int64_t
      ; CXType_OCLImage2dMSAADepthRW, constant "CXType_OCLImage2dMSAADepthRW" int64_t
      ; ( CXType_OCLImage2dArrayMSAADepthRW
        , constant "CXType_OCLImage2dArrayMSAADepthRW" int64_t )
      ; CXType_OCLImage3dRW, constant "CXType_OCLImage3dRW" int64_t
      ; CXType_OCLSampler, constant "CXType_OCLSampler" int64_t
      ; CXType_OCLEvent, constant "CXType_OCLEvent" int64_t
      ; CXType_OCLQueue, constant "CXType_OCLQueue" int64_t
      ; CXType_OCLReserveID, constant "CXType_OCLReserveID" int64_t
      ; CXType_ObjCObject, constant "CXType_ObjCObject" int64_t
      ; CXType_ObjCTypeParam, constant "CXType_ObjCTypeParam" int64_t
      ; CXType_Attributed, constant "CXType_Attributed" int64_t
      ; ( CXType_OCLIntelSubgroupAVCMcePayload
        , constant "CXType_OCLIntelSubgroupAVCMcePayload" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImePayload
        , constant "CXType_OCLIntelSubgroupAVCImePayload" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCRefPayload
        , constant "CXType_OCLIntelSubgroupAVCRefPayload" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCSicPayload
        , constant "CXType_OCLIntelSubgroupAVCSicPayload" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCMceResult
        , constant "CXType_OCLIntelSubgroupAVCMceResult" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeResult
        , constant "CXType_OCLIntelSubgroupAVCImeResult" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCRefResult
        , constant "CXType_OCLIntelSubgroupAVCRefResult" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCSicResult
        , constant "CXType_OCLIntelSubgroupAVCSicResult" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeResultSingleReferenceStreamout
        , constant "CXType_OCLIntelSubgroupAVCImeResultSingleReferenceStreamout" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeResultDualReferenceStreamout
        , constant "CXType_OCLIntelSubgroupAVCImeResultDualReferenceStreamout" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeSingleReferenceStreamin
        , constant "CXType_OCLIntelSubgroupAVCImeSingleReferenceStreamin" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeDualReferenceStreamin
        , constant "CXType_OCLIntelSubgroupAVCImeDualReferenceStreamin" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout
        , constant "CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout
        , constant "CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeSingleRefStreamin
        , constant "CXType_OCLIntelSubgroupAVCImeSingleRefStreamin" int64_t )
      ; ( CXType_OCLIntelSubgroupAVCImeDualRefStreamin
        , constant "CXType_OCLIntelSubgroupAVCImeDualRefStreamin" int64_t )
      ; CXType_ExtVector, constant "CXType_ExtVector" int64_t
      ; CXType_Atomic, constant "CXType_Atomic" int64_t
      ; CXType_BTFTagAttributed, constant "CXType_BTFTagAttributed" int64_t
      ]


    let t : t typ =
      enum "CXTypeKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXCallingConv = struct
    type t =
      | CXCallingConv_Default
      | CXCallingConv_C
      | CXCallingConv_X86StdCall
      | CXCallingConv_X86FastCall
      | CXCallingConv_X86ThisCall
      | CXCallingConv_X86Pascal
      | CXCallingConv_AAPCS
      | CXCallingConv_AAPCS_VFP
      | CXCallingConv_X86RegCall
      | CXCallingConv_IntelOclBicc
      | CXCallingConv_Win64
      | CXCallingConv_X86_64Win64
      | CXCallingConv_X86_64SysV
      | CXCallingConv_X86VectorCall
      | CXCallingConv_Swift
      | CXCallingConv_PreserveMost
      | CXCallingConv_PreserveAll
      | CXCallingConv_AArch64VectorCall
      | CXCallingConv_SwiftAsync
      | CXCallingConv_AArch64SVEPCS
      | CXCallingConv_M68kRTD
      | CXCallingConv_PreserveNone
      | CXCallingConv_RISCVVectorCall
      | CXCallingConv_Invalid
      | CXCallingConv_Unexposed

    let mapping =
      [ CXCallingConv_Default, constant "CXCallingConv_Default" int64_t
      ; CXCallingConv_C, constant "CXCallingConv_C" int64_t
      ; CXCallingConv_X86StdCall, constant "CXCallingConv_X86StdCall" int64_t
      ; CXCallingConv_X86FastCall, constant "CXCallingConv_X86FastCall" int64_t
      ; CXCallingConv_X86ThisCall, constant "CXCallingConv_X86ThisCall" int64_t
      ; CXCallingConv_X86Pascal, constant "CXCallingConv_X86Pascal" int64_t
      ; CXCallingConv_AAPCS, constant "CXCallingConv_AAPCS" int64_t
      ; CXCallingConv_AAPCS_VFP, constant "CXCallingConv_AAPCS_VFP" int64_t
      ; CXCallingConv_X86RegCall, constant "CXCallingConv_X86RegCall" int64_t
      ; CXCallingConv_IntelOclBicc, constant "CXCallingConv_IntelOclBicc" int64_t
      ; CXCallingConv_Win64, constant "CXCallingConv_Win64" int64_t
      ; CXCallingConv_X86_64Win64, constant "CXCallingConv_X86_64Win64" int64_t
      ; CXCallingConv_X86_64SysV, constant "CXCallingConv_X86_64SysV" int64_t
      ; CXCallingConv_X86VectorCall, constant "CXCallingConv_X86VectorCall" int64_t
      ; CXCallingConv_Swift, constant "CXCallingConv_Swift" int64_t
      ; CXCallingConv_PreserveMost, constant "CXCallingConv_PreserveMost" int64_t
      ; CXCallingConv_PreserveAll, constant "CXCallingConv_PreserveAll" int64_t
      ; ( CXCallingConv_AArch64VectorCall
        , constant "CXCallingConv_AArch64VectorCall" int64_t )
      ; CXCallingConv_SwiftAsync, constant "CXCallingConv_SwiftAsync" int64_t
      ; CXCallingConv_AArch64SVEPCS, constant "CXCallingConv_AArch64SVEPCS" int64_t
      ; CXCallingConv_M68kRTD, constant "CXCallingConv_M68kRTD" int64_t
      ; CXCallingConv_PreserveNone, constant "CXCallingConv_PreserveNone" int64_t
      ; CXCallingConv_RISCVVectorCall, constant "CXCallingConv_RISCVVectorCall" int64_t
      ; CXCallingConv_Invalid, constant "CXCallingConv_Invalid" int64_t
      ; CXCallingConv_Unexposed, constant "CXCallingConv_Unexposed" int64_t
      ]


    let t : t typ =
      enum "CXCallingConv" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXType = struct
    type t

    let t : t structure typ = typedef (structure "CXType") "CXType"
    let kind = field t "kind" CXTypeKind.t
    let data = field t "data" (array 2 (ptr void))
    let () = seal t
  end

  module CXTemplateArgumentKind = struct
    type t =
      | CXTemplateArgumentKind_Null
      | CXTemplateArgumentKind_Type
      | CXTemplateArgumentKind_Declaration
      | CXTemplateArgumentKind_NullPtr
      | CXTemplateArgumentKind_Integral
      | CXTemplateArgumentKind_Template
      | CXTemplateArgumentKind_TemplateExpansion
      | CXTemplateArgumentKind_Expression
      | CXTemplateArgumentKind_Pack
      | CXTemplateArgumentKind_Invalid

    let mapping =
      [ CXTemplateArgumentKind_Null, constant "CXTemplateArgumentKind_Null" int64_t
      ; CXTemplateArgumentKind_Type, constant "CXTemplateArgumentKind_Type" int64_t
      ; ( CXTemplateArgumentKind_Declaration
        , constant "CXTemplateArgumentKind_Declaration" int64_t )
      ; CXTemplateArgumentKind_NullPtr, constant "CXTemplateArgumentKind_NullPtr" int64_t
      ; ( CXTemplateArgumentKind_Integral
        , constant "CXTemplateArgumentKind_Integral" int64_t )
      ; ( CXTemplateArgumentKind_Template
        , constant "CXTemplateArgumentKind_Template" int64_t )
      ; ( CXTemplateArgumentKind_TemplateExpansion
        , constant "CXTemplateArgumentKind_TemplateExpansion" int64_t )
      ; ( CXTemplateArgumentKind_Expression
        , constant "CXTemplateArgumentKind_Expression" int64_t )
      ; CXTemplateArgumentKind_Pack, constant "CXTemplateArgumentKind_Pack" int64_t
      ; CXTemplateArgumentKind_Invalid, constant "CXTemplateArgumentKind_Invalid" int64_t
      ]


    let t : t typ =
      enum "CXTemplateArgumentKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXTypeNullabilityKind = struct
    type t =
      | CXTypeNullability_NonNull
      | CXTypeNullability_Nullable
      | CXTypeNullability_Unspecified
      | CXTypeNullability_Invalid
      | CXTypeNullability_NullableResult

    let mapping =
      [ CXTypeNullability_NonNull, constant "CXTypeNullability_NonNull" int64_t
      ; CXTypeNullability_Nullable, constant "CXTypeNullability_Nullable" int64_t
      ; CXTypeNullability_Unspecified, constant "CXTypeNullability_Unspecified" int64_t
      ; CXTypeNullability_Invalid, constant "CXTypeNullability_Invalid" int64_t
      ; ( CXTypeNullability_NullableResult
        , constant "CXTypeNullability_NullableResult" int64_t )
      ]


    let t : t typ =
      enum "CXTypeNullabilityKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXTypeLayoutError = struct
    type t =
      | CXTypeLayoutError_Invalid
      | CXTypeLayoutError_Incomplete
      | CXTypeLayoutError_Dependent
      | CXTypeLayoutError_NotConstantSize
      | CXTypeLayoutError_InvalidFieldName
      | CXTypeLayoutError_Undeduced

    let mapping =
      [ CXTypeLayoutError_Invalid, constant "CXTypeLayoutError_Invalid" int64_t
      ; CXTypeLayoutError_Incomplete, constant "CXTypeLayoutError_Incomplete" int64_t
      ; CXTypeLayoutError_Dependent, constant "CXTypeLayoutError_Dependent" int64_t
      ; ( CXTypeLayoutError_NotConstantSize
        , constant "CXTypeLayoutError_NotConstantSize" int64_t )
      ; ( CXTypeLayoutError_InvalidFieldName
        , constant "CXTypeLayoutError_InvalidFieldName" int64_t )
      ; CXTypeLayoutError_Undeduced, constant "CXTypeLayoutError_Undeduced" int64_t
      ]


    let t : t typ =
      enum "CXTypeLayoutError" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXRefQualifierKind = struct
    type t =
      | CXRefQualifier_None
      | CXRefQualifier_LValue
      | CXRefQualifier_RValue

    let mapping =
      [ CXRefQualifier_None, constant "CXRefQualifier_None" int64_t
      ; CXRefQualifier_LValue, constant "CXRefQualifier_LValue" int64_t
      ; CXRefQualifier_RValue, constant "CXRefQualifier_RValue" int64_t
      ]


    let t : t typ =
      enum "CXRefQualifierKind" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CX_CXXAccessSpecifier = struct
    type t =
      | CX_CXXInvalidAccessSpecifier
      | CX_CXXPublic
      | CX_CXXProtected
      | CX_CXXPrivate

    let mapping =
      [ CX_CXXInvalidAccessSpecifier, constant "CX_CXXInvalidAccessSpecifier" int64_t
      ; CX_CXXPublic, constant "CX_CXXPublic" int64_t
      ; CX_CXXProtected, constant "CX_CXXProtected" int64_t
      ; CX_CXXPrivate, constant "CX_CXXPrivate" int64_t
      ]


    let t : t typ =
      enum "CX_CXXAccessSpecifier" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CX_StorageClass = struct
    type t =
      | CX_SC_Invalid
      | CX_SC_None
      | CX_SC_Extern
      | CX_SC_Static
      | CX_SC_PrivateExtern
      | CX_SC_OpenCLWorkGroupLocal
      | CX_SC_Auto
      | CX_SC_Register

    let mapping =
      [ CX_SC_Invalid, constant "CX_SC_Invalid" int64_t
      ; CX_SC_None, constant "CX_SC_None" int64_t
      ; CX_SC_Extern, constant "CX_SC_Extern" int64_t
      ; CX_SC_Static, constant "CX_SC_Static" int64_t
      ; CX_SC_PrivateExtern, constant "CX_SC_PrivateExtern" int64_t
      ; CX_SC_OpenCLWorkGroupLocal, constant "CX_SC_OpenCLWorkGroupLocal" int64_t
      ; CX_SC_Auto, constant "CX_SC_Auto" int64_t
      ; CX_SC_Register, constant "CX_SC_Register" int64_t
      ]


    let t : t typ =
      enum "CX_StorageClass" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CX_BinaryOperatorKind = struct
    type t =
      | CX_BO_Invalid
      | CX_BO_PtrMemD
      | CX_BO_PtrMemI
      | CX_BO_Mul
      | CX_BO_Div
      | CX_BO_Rem
      | CX_BO_Add
      | CX_BO_Sub
      | CX_BO_Shl
      | CX_BO_Shr
      | CX_BO_Cmp
      | CX_BO_LT
      | CX_BO_GT
      | CX_BO_LE
      | CX_BO_GE
      | CX_BO_EQ
      | CX_BO_NE
      | CX_BO_And
      | CX_BO_Xor
      | CX_BO_Or
      | CX_BO_LAnd
      | CX_BO_LOr
      | CX_BO_Assign
      | CX_BO_MulAssign
      | CX_BO_DivAssign
      | CX_BO_RemAssign
      | CX_BO_AddAssign
      | CX_BO_SubAssign
      | CX_BO_ShlAssign
      | CX_BO_ShrAssign
      | CX_BO_AndAssign
      | CX_BO_XorAssign
      | CX_BO_OrAssign
      | CX_BO_Comma
      | CX_BO_LAST

    let mapping =
      [ CX_BO_Invalid, constant "CX_BO_Invalid" int64_t
      ; CX_BO_PtrMemD, constant "CX_BO_PtrMemD" int64_t
      ; CX_BO_PtrMemI, constant "CX_BO_PtrMemI" int64_t
      ; CX_BO_Mul, constant "CX_BO_Mul" int64_t
      ; CX_BO_Div, constant "CX_BO_Div" int64_t
      ; CX_BO_Rem, constant "CX_BO_Rem" int64_t
      ; CX_BO_Add, constant "CX_BO_Add" int64_t
      ; CX_BO_Sub, constant "CX_BO_Sub" int64_t
      ; CX_BO_Shl, constant "CX_BO_Shl" int64_t
      ; CX_BO_Shr, constant "CX_BO_Shr" int64_t
      ; CX_BO_Cmp, constant "CX_BO_Cmp" int64_t
      ; CX_BO_LT, constant "CX_BO_LT" int64_t
      ; CX_BO_GT, constant "CX_BO_GT" int64_t
      ; CX_BO_LE, constant "CX_BO_LE" int64_t
      ; CX_BO_GE, constant "CX_BO_GE" int64_t
      ; CX_BO_EQ, constant "CX_BO_EQ" int64_t
      ; CX_BO_NE, constant "CX_BO_NE" int64_t
      ; CX_BO_And, constant "CX_BO_And" int64_t
      ; CX_BO_Xor, constant "CX_BO_Xor" int64_t
      ; CX_BO_Or, constant "CX_BO_Or" int64_t
      ; CX_BO_LAnd, constant "CX_BO_LAnd" int64_t
      ; CX_BO_LOr, constant "CX_BO_LOr" int64_t
      ; CX_BO_Assign, constant "CX_BO_Assign" int64_t
      ; CX_BO_MulAssign, constant "CX_BO_MulAssign" int64_t
      ; CX_BO_DivAssign, constant "CX_BO_DivAssign" int64_t
      ; CX_BO_RemAssign, constant "CX_BO_RemAssign" int64_t
      ; CX_BO_AddAssign, constant "CX_BO_AddAssign" int64_t
      ; CX_BO_SubAssign, constant "CX_BO_SubAssign" int64_t
      ; CX_BO_ShlAssign, constant "CX_BO_ShlAssign" int64_t
      ; CX_BO_ShrAssign, constant "CX_BO_ShrAssign" int64_t
      ; CX_BO_AndAssign, constant "CX_BO_AndAssign" int64_t
      ; CX_BO_XorAssign, constant "CX_BO_XorAssign" int64_t
      ; CX_BO_OrAssign, constant "CX_BO_OrAssign" int64_t
      ; CX_BO_Comma, constant "CX_BO_Comma" int64_t
      ; CX_BO_LAST, constant "CX_BO_LAST" int64_t
      ]


    let t : t typ =
      enum "CX_BinaryOperatorKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXChildVisitResult = struct
    type t =
      | CXChildVisit_Break
      | CXChildVisit_Continue
      | CXChildVisit_Recurse

    let mapping =
      [ CXChildVisit_Break, constant "CXChildVisit_Break" int64_t
      ; CXChildVisit_Continue, constant "CXChildVisit_Continue" int64_t
      ; CXChildVisit_Recurse, constant "CXChildVisit_Recurse" int64_t
      ]


    let t : t typ =
      enum "CXChildVisitResult" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXCursorVisitor = struct
    type t =
      CXCursor.t structure ptr
      -> CXCursor.t structure ptr
      -> CXClientData.t
      -> CXChildVisitResult.t

    let t : t static_funptr typ =
      typedef
        (static_funptr
           (ptr (const CXCursor.t)
            @-> ptr (const CXCursor.t)
            @-> CXClientData.t
            @-> returning CXChildVisitResult.t))
        "CXCursorVisitor"
  end

  module CXCursorVisitorBlock = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXCursorVisitorBlock"
  end

  module CXPrintingPolicy = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXPrintingPolicy"
  end

  module CXPrintingPolicyProperty = struct
    type t =
      | CXPrintingPolicy_Indentation
      | CXPrintingPolicy_SuppressSpecifiers
      | CXPrintingPolicy_SuppressTagKeyword
      | CXPrintingPolicy_IncludeTagDefinition
      | CXPrintingPolicy_SuppressScope
      | CXPrintingPolicy_SuppressUnwrittenScope
      | CXPrintingPolicy_SuppressInitializers
      | CXPrintingPolicy_ConstantArraySizeAsWritten
      | CXPrintingPolicy_AnonymousTagLocations
      | CXPrintingPolicy_SuppressStrongLifetime
      | CXPrintingPolicy_SuppressLifetimeQualifiers
      | CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors
      | CXPrintingPolicy_Bool
      | CXPrintingPolicy_Restrict
      | CXPrintingPolicy_Alignof
      | CXPrintingPolicy_UnderscoreAlignof
      | CXPrintingPolicy_UseVoidForZeroParams
      | CXPrintingPolicy_TerseOutput
      | CXPrintingPolicy_PolishForDeclaration
      | CXPrintingPolicy_Half
      | CXPrintingPolicy_MSWChar
      | CXPrintingPolicy_IncludeNewlines
      | CXPrintingPolicy_MSVCFormatting
      | CXPrintingPolicy_ConstantsAsWritten
      | CXPrintingPolicy_SuppressImplicitBase
      | CXPrintingPolicy_FullyQualifiedName
      | CXPrintingPolicy_LastProperty

    let mapping =
      [ CXPrintingPolicy_Indentation, constant "CXPrintingPolicy_Indentation" int64_t
      ; ( CXPrintingPolicy_SuppressSpecifiers
        , constant "CXPrintingPolicy_SuppressSpecifiers" int64_t )
      ; ( CXPrintingPolicy_SuppressTagKeyword
        , constant "CXPrintingPolicy_SuppressTagKeyword" int64_t )
      ; ( CXPrintingPolicy_IncludeTagDefinition
        , constant "CXPrintingPolicy_IncludeTagDefinition" int64_t )
      ; CXPrintingPolicy_SuppressScope, constant "CXPrintingPolicy_SuppressScope" int64_t
      ; ( CXPrintingPolicy_SuppressUnwrittenScope
        , constant "CXPrintingPolicy_SuppressUnwrittenScope" int64_t )
      ; ( CXPrintingPolicy_SuppressInitializers
        , constant "CXPrintingPolicy_SuppressInitializers" int64_t )
      ; ( CXPrintingPolicy_ConstantArraySizeAsWritten
        , constant "CXPrintingPolicy_ConstantArraySizeAsWritten" int64_t )
      ; ( CXPrintingPolicy_AnonymousTagLocations
        , constant "CXPrintingPolicy_AnonymousTagLocations" int64_t )
      ; ( CXPrintingPolicy_SuppressStrongLifetime
        , constant "CXPrintingPolicy_SuppressStrongLifetime" int64_t )
      ; ( CXPrintingPolicy_SuppressLifetimeQualifiers
        , constant "CXPrintingPolicy_SuppressLifetimeQualifiers" int64_t )
      ; ( CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors
        , constant "CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors" int64_t )
      ; CXPrintingPolicy_Bool, constant "CXPrintingPolicy_Bool" int64_t
      ; CXPrintingPolicy_Restrict, constant "CXPrintingPolicy_Restrict" int64_t
      ; CXPrintingPolicy_Alignof, constant "CXPrintingPolicy_Alignof" int64_t
      ; ( CXPrintingPolicy_UnderscoreAlignof
        , constant "CXPrintingPolicy_UnderscoreAlignof" int64_t )
      ; ( CXPrintingPolicy_UseVoidForZeroParams
        , constant "CXPrintingPolicy_UseVoidForZeroParams" int64_t )
      ; CXPrintingPolicy_TerseOutput, constant "CXPrintingPolicy_TerseOutput" int64_t
      ; ( CXPrintingPolicy_PolishForDeclaration
        , constant "CXPrintingPolicy_PolishForDeclaration" int64_t )
      ; CXPrintingPolicy_Half, constant "CXPrintingPolicy_Half" int64_t
      ; CXPrintingPolicy_MSWChar, constant "CXPrintingPolicy_MSWChar" int64_t
      ; ( CXPrintingPolicy_IncludeNewlines
        , constant "CXPrintingPolicy_IncludeNewlines" int64_t )
      ; ( CXPrintingPolicy_MSVCFormatting
        , constant "CXPrintingPolicy_MSVCFormatting" int64_t )
      ; ( CXPrintingPolicy_ConstantsAsWritten
        , constant "CXPrintingPolicy_ConstantsAsWritten" int64_t )
      ; ( CXPrintingPolicy_SuppressImplicitBase
        , constant "CXPrintingPolicy_SuppressImplicitBase" int64_t )
      ; ( CXPrintingPolicy_FullyQualifiedName
        , constant "CXPrintingPolicy_FullyQualifiedName" int64_t )
      ; CXPrintingPolicy_LastProperty, constant "CXPrintingPolicy_LastProperty" int64_t
      ]


    let t : t typ =
      enum "CXPrintingPolicyProperty" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXObjCPropertyAttrKind = struct
    type t =
      | CXObjCPropertyAttr_noattr
      | CXObjCPropertyAttr_readonly
      | CXObjCPropertyAttr_getter
      | CXObjCPropertyAttr_assign
      | CXObjCPropertyAttr_readwrite
      | CXObjCPropertyAttr_retain
      | CXObjCPropertyAttr_copy
      | CXObjCPropertyAttr_nonatomic
      | CXObjCPropertyAttr_setter
      | CXObjCPropertyAttr_atomic
      | CXObjCPropertyAttr_weak
      | CXObjCPropertyAttr_strong
      | CXObjCPropertyAttr_unsafe_unretained
      | CXObjCPropertyAttr_class

    let mapping =
      [ CXObjCPropertyAttr_noattr, constant "CXObjCPropertyAttr_noattr" int64_t
      ; CXObjCPropertyAttr_readonly, constant "CXObjCPropertyAttr_readonly" int64_t
      ; CXObjCPropertyAttr_getter, constant "CXObjCPropertyAttr_getter" int64_t
      ; CXObjCPropertyAttr_assign, constant "CXObjCPropertyAttr_assign" int64_t
      ; CXObjCPropertyAttr_readwrite, constant "CXObjCPropertyAttr_readwrite" int64_t
      ; CXObjCPropertyAttr_retain, constant "CXObjCPropertyAttr_retain" int64_t
      ; CXObjCPropertyAttr_copy, constant "CXObjCPropertyAttr_copy" int64_t
      ; CXObjCPropertyAttr_nonatomic, constant "CXObjCPropertyAttr_nonatomic" int64_t
      ; CXObjCPropertyAttr_setter, constant "CXObjCPropertyAttr_setter" int64_t
      ; CXObjCPropertyAttr_atomic, constant "CXObjCPropertyAttr_atomic" int64_t
      ; CXObjCPropertyAttr_weak, constant "CXObjCPropertyAttr_weak" int64_t
      ; CXObjCPropertyAttr_strong, constant "CXObjCPropertyAttr_strong" int64_t
      ; ( CXObjCPropertyAttr_unsafe_unretained
        , constant "CXObjCPropertyAttr_unsafe_unretained" int64_t )
      ; CXObjCPropertyAttr_class, constant "CXObjCPropertyAttr_class" int64_t
      ]


    let t : t typ =
      enum "CXObjCPropertyAttrKind" ~typedef:true mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXObjCDeclQualifierKind = struct
    type t =
      | CXObjCDeclQualifier_None
      | CXObjCDeclQualifier_In
      | CXObjCDeclQualifier_Inout
      | CXObjCDeclQualifier_Out
      | CXObjCDeclQualifier_Bycopy
      | CXObjCDeclQualifier_Byref
      | CXObjCDeclQualifier_Oneway

    let mapping =
      [ CXObjCDeclQualifier_None, constant "CXObjCDeclQualifier_None" int64_t
      ; CXObjCDeclQualifier_In, constant "CXObjCDeclQualifier_In" int64_t
      ; CXObjCDeclQualifier_Inout, constant "CXObjCDeclQualifier_Inout" int64_t
      ; CXObjCDeclQualifier_Out, constant "CXObjCDeclQualifier_Out" int64_t
      ; CXObjCDeclQualifier_Bycopy, constant "CXObjCDeclQualifier_Bycopy" int64_t
      ; CXObjCDeclQualifier_Byref, constant "CXObjCDeclQualifier_Byref" int64_t
      ; CXObjCDeclQualifier_Oneway, constant "CXObjCDeclQualifier_Oneway" int64_t
      ]


    let t : t typ =
      enum "CXObjCDeclQualifierKind" ~typedef:true mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXModule = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXModule"
  end

  module CXNameRefFlags = struct
    type t =
      | CXNameRange_WantQualifier
      | CXNameRange_WantTemplateArgs
      | CXNameRange_WantSinglePiece

    let mapping =
      [ CXNameRange_WantQualifier, constant "CXNameRange_WantQualifier" int64_t
      ; CXNameRange_WantTemplateArgs, constant "CXNameRange_WantTemplateArgs" int64_t
      ; CXNameRange_WantSinglePiece, constant "CXNameRange_WantSinglePiece" int64_t
      ]


    let t : t typ =
      enum "CXNameRefFlags" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXTokenKind = struct
    type t =
      | CXToken_Punctuation
      | CXToken_Keyword
      | CXToken_Identifier
      | CXToken_Literal
      | CXToken_Comment

    let mapping =
      [ CXToken_Punctuation, constant "CXToken_Punctuation" int64_t
      ; CXToken_Keyword, constant "CXToken_Keyword" int64_t
      ; CXToken_Identifier, constant "CXToken_Identifier" int64_t
      ; CXToken_Literal, constant "CXToken_Literal" int64_t
      ; CXToken_Comment, constant "CXToken_Comment" int64_t
      ]


    let t : t typ =
      enum "CXTokenKind" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXToken = struct
    type t

    let t : t structure typ = typedef (structure "CXToken") "CXToken"
    let int_data = field t "int_data" (array 4 uint)
    let ptr_data = field t "ptr_data" (ptr void)
    let () = seal t
  end

  module CXCompletionString = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXCompletionString"
  end

  module CXCompletionResult = struct
    type t

    let t : t structure typ =
      typedef (structure "CXCompletionResult") "CXCompletionResult"


    let cursor_kind = field t "CursorKind" CXCursorKind.t
    let completion_string = field t "CompletionString" CXCompletionString.t
    let () = seal t
  end

  module CXCompletionChunkKind = struct
    type t =
      | CXCompletionChunk_Optional
      | CXCompletionChunk_TypedText
      | CXCompletionChunk_Text
      | CXCompletionChunk_Placeholder
      | CXCompletionChunk_Informative
      | CXCompletionChunk_CurrentParameter
      | CXCompletionChunk_LeftParen
      | CXCompletionChunk_RightParen
      | CXCompletionChunk_LeftBracket
      | CXCompletionChunk_RightBracket
      | CXCompletionChunk_LeftBrace
      | CXCompletionChunk_RightBrace
      | CXCompletionChunk_LeftAngle
      | CXCompletionChunk_RightAngle
      | CXCompletionChunk_Comma
      | CXCompletionChunk_ResultType
      | CXCompletionChunk_Colon
      | CXCompletionChunk_SemiColon
      | CXCompletionChunk_Equal
      | CXCompletionChunk_HorizontalSpace
      | CXCompletionChunk_VerticalSpace

    let mapping =
      [ CXCompletionChunk_Optional, constant "CXCompletionChunk_Optional" int64_t
      ; CXCompletionChunk_TypedText, constant "CXCompletionChunk_TypedText" int64_t
      ; CXCompletionChunk_Text, constant "CXCompletionChunk_Text" int64_t
      ; CXCompletionChunk_Placeholder, constant "CXCompletionChunk_Placeholder" int64_t
      ; CXCompletionChunk_Informative, constant "CXCompletionChunk_Informative" int64_t
      ; ( CXCompletionChunk_CurrentParameter
        , constant "CXCompletionChunk_CurrentParameter" int64_t )
      ; CXCompletionChunk_LeftParen, constant "CXCompletionChunk_LeftParen" int64_t
      ; CXCompletionChunk_RightParen, constant "CXCompletionChunk_RightParen" int64_t
      ; CXCompletionChunk_LeftBracket, constant "CXCompletionChunk_LeftBracket" int64_t
      ; CXCompletionChunk_RightBracket, constant "CXCompletionChunk_RightBracket" int64_t
      ; CXCompletionChunk_LeftBrace, constant "CXCompletionChunk_LeftBrace" int64_t
      ; CXCompletionChunk_RightBrace, constant "CXCompletionChunk_RightBrace" int64_t
      ; CXCompletionChunk_LeftAngle, constant "CXCompletionChunk_LeftAngle" int64_t
      ; CXCompletionChunk_RightAngle, constant "CXCompletionChunk_RightAngle" int64_t
      ; CXCompletionChunk_Comma, constant "CXCompletionChunk_Comma" int64_t
      ; CXCompletionChunk_ResultType, constant "CXCompletionChunk_ResultType" int64_t
      ; CXCompletionChunk_Colon, constant "CXCompletionChunk_Colon" int64_t
      ; CXCompletionChunk_SemiColon, constant "CXCompletionChunk_SemiColon" int64_t
      ; CXCompletionChunk_Equal, constant "CXCompletionChunk_Equal" int64_t
      ; ( CXCompletionChunk_HorizontalSpace
        , constant "CXCompletionChunk_HorizontalSpace" int64_t )
      ; ( CXCompletionChunk_VerticalSpace
        , constant "CXCompletionChunk_VerticalSpace" int64_t )
      ]


    let t : t typ =
      enum "CXCompletionChunkKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXCodeCompleteResults = struct
    type t

    let t : t structure typ =
      typedef (structure "CXCodeCompleteResults") "CXCodeCompleteResults"


    let results = field t "Results" (ptr CXCompletionResult.t)
    let num_results = field t "NumResults" uint
    let () = seal t
  end

  module CXCodeComplete_Flags = struct
    type t =
      | CXCodeComplete_IncludeMacros
      | CXCodeComplete_IncludeCodePatterns
      | CXCodeComplete_IncludeBriefComments
      | CXCodeComplete_SkipPreamble
      | CXCodeComplete_IncludeCompletionsWithFixIts

    let mapping =
      [ CXCodeComplete_IncludeMacros, constant "CXCodeComplete_IncludeMacros" int64_t
      ; ( CXCodeComplete_IncludeCodePatterns
        , constant "CXCodeComplete_IncludeCodePatterns" int64_t )
      ; ( CXCodeComplete_IncludeBriefComments
        , constant "CXCodeComplete_IncludeBriefComments" int64_t )
      ; CXCodeComplete_SkipPreamble, constant "CXCodeComplete_SkipPreamble" int64_t
      ; ( CXCodeComplete_IncludeCompletionsWithFixIts
        , constant "CXCodeComplete_IncludeCompletionsWithFixIts" int64_t )
      ]


    let t : t typ =
      enum "CXCodeComplete_Flags" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXCompletionContext = struct
    type t =
      | CXCompletionContext_Unexposed
      | CXCompletionContext_AnyType
      | CXCompletionContext_AnyValue
      | CXCompletionContext_ObjCObjectValue
      | CXCompletionContext_ObjCSelectorValue
      | CXCompletionContext_CXXClassTypeValue
      | CXCompletionContext_DotMemberAccess
      | CXCompletionContext_ArrowMemberAccess
      | CXCompletionContext_ObjCPropertyAccess
      | CXCompletionContext_EnumTag
      | CXCompletionContext_UnionTag
      | CXCompletionContext_StructTag
      | CXCompletionContext_ClassTag
      | CXCompletionContext_Namespace
      | CXCompletionContext_NestedNameSpecifier
      | CXCompletionContext_ObjCInterface
      | CXCompletionContext_ObjCProtocol
      | CXCompletionContext_ObjCCategory
      | CXCompletionContext_ObjCInstanceMessage
      | CXCompletionContext_ObjCClassMessage
      | CXCompletionContext_ObjCSelectorName
      | CXCompletionContext_MacroName
      | CXCompletionContext_NaturalLanguage
      | CXCompletionContext_IncludedFile
      | CXCompletionContext_Unknown

    let mapping =
      [ CXCompletionContext_Unexposed, constant "CXCompletionContext_Unexposed" int64_t
      ; CXCompletionContext_AnyType, constant "CXCompletionContext_AnyType" int64_t
      ; CXCompletionContext_AnyValue, constant "CXCompletionContext_AnyValue" int64_t
      ; ( CXCompletionContext_ObjCObjectValue
        , constant "CXCompletionContext_ObjCObjectValue" int64_t )
      ; ( CXCompletionContext_ObjCSelectorValue
        , constant "CXCompletionContext_ObjCSelectorValue" int64_t )
      ; ( CXCompletionContext_CXXClassTypeValue
        , constant "CXCompletionContext_CXXClassTypeValue" int64_t )
      ; ( CXCompletionContext_DotMemberAccess
        , constant "CXCompletionContext_DotMemberAccess" int64_t )
      ; ( CXCompletionContext_ArrowMemberAccess
        , constant "CXCompletionContext_ArrowMemberAccess" int64_t )
      ; ( CXCompletionContext_ObjCPropertyAccess
        , constant "CXCompletionContext_ObjCPropertyAccess" int64_t )
      ; CXCompletionContext_EnumTag, constant "CXCompletionContext_EnumTag" int64_t
      ; CXCompletionContext_UnionTag, constant "CXCompletionContext_UnionTag" int64_t
      ; CXCompletionContext_StructTag, constant "CXCompletionContext_StructTag" int64_t
      ; CXCompletionContext_ClassTag, constant "CXCompletionContext_ClassTag" int64_t
      ; CXCompletionContext_Namespace, constant "CXCompletionContext_Namespace" int64_t
      ; ( CXCompletionContext_NestedNameSpecifier
        , constant "CXCompletionContext_NestedNameSpecifier" int64_t )
      ; ( CXCompletionContext_ObjCInterface
        , constant "CXCompletionContext_ObjCInterface" int64_t )
      ; ( CXCompletionContext_ObjCProtocol
        , constant "CXCompletionContext_ObjCProtocol" int64_t )
      ; ( CXCompletionContext_ObjCCategory
        , constant "CXCompletionContext_ObjCCategory" int64_t )
      ; ( CXCompletionContext_ObjCInstanceMessage
        , constant "CXCompletionContext_ObjCInstanceMessage" int64_t )
      ; ( CXCompletionContext_ObjCClassMessage
        , constant "CXCompletionContext_ObjCClassMessage" int64_t )
      ; ( CXCompletionContext_ObjCSelectorName
        , constant "CXCompletionContext_ObjCSelectorName" int64_t )
      ; CXCompletionContext_MacroName, constant "CXCompletionContext_MacroName" int64_t
      ; ( CXCompletionContext_NaturalLanguage
        , constant "CXCompletionContext_NaturalLanguage" int64_t )
      ; ( CXCompletionContext_IncludedFile
        , constant "CXCompletionContext_IncludedFile" int64_t )
      ; CXCompletionContext_Unknown, constant "CXCompletionContext_Unknown" int64_t
      ]


    let t : t typ =
      enum "CXCompletionContext" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXFile = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXFile"
  end

  module CXSourceLocation = struct
    type t

    let t : t structure typ = typedef (structure "CXSourceLocation") "CXSourceLocation"
    let ptr_data = field t "ptr_data" (array 2 (ptr (const void)))
    let int_data = field t "int_data" uint
    let () = seal t
  end

  module CXInclusionVisitor = struct
    type t =
      CXFile.t
      -> CXSourceLocation.t structure ptr
      -> Unsigned.uint
      -> CXClientData.t
      -> unit

    let t : t static_funptr typ =
      typedef
        (static_funptr
           (CXFile.t
            @-> ptr CXSourceLocation.t
            @-> uint
            @-> CXClientData.t
            @-> returning void))
        "CXInclusionVisitor"
  end

  module CXEvalResultKind = struct
    type t =
      | CXEval_Int
      | CXEval_Float
      | CXEval_ObjCStrLiteral
      | CXEval_StrLiteral
      | CXEval_CFStr
      | CXEval_Other
      | CXEval_UnExposed

    let mapping =
      [ CXEval_Int, constant "CXEval_Int" int64_t
      ; CXEval_Float, constant "CXEval_Float" int64_t
      ; CXEval_ObjCStrLiteral, constant "CXEval_ObjCStrLiteral" int64_t
      ; CXEval_StrLiteral, constant "CXEval_StrLiteral" int64_t
      ; CXEval_CFStr, constant "CXEval_CFStr" int64_t
      ; CXEval_Other, constant "CXEval_Other" int64_t
      ; CXEval_UnExposed, constant "CXEval_UnExposed" int64_t
      ]


    let t : t typ =
      enum "CXEvalResultKind" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXEvalResult = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXEvalResult"
  end

  module CXRemapping = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXRemapping"
  end

  module CXVisitorResult = struct
    type t =
      | CXVisit_Break
      | CXVisit_Continue

    let mapping =
      [ CXVisit_Break, constant "CXVisit_Break" int64_t
      ; CXVisit_Continue, constant "CXVisit_Continue" int64_t
      ]


    let t : t typ =
      enum "CXVisitorResult" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXSourceRange = struct
    type t

    let t : t structure typ = typedef (structure "CXSourceRange") "CXSourceRange"
    let ptr_data = field t "ptr_data" (ptr (ptr (const void)))
    let begin_int_data = field t "begin_int_data" uint
    let end_int_data = field t "end_int_data" uint
    let () = seal t
  end

  module CXCursorAndRangeVisitor = struct
    type t

    let t : t structure typ =
      typedef (structure "CXCursorAndRangeVisitor") "CXCursorAndRangeVisitor"


    let context = field t "context" (ptr void)

    let visit =
      field
        t
        "visit"
        (static_funptr
           (ptr void
            @-> ptr (const CXCursor.t)
            @-> ptr (const CXSourceRange.t)
            @-> returning CXVisitorResult.t))


    let () = seal t
  end

  module CXResult = struct
    type t =
      | CXResult_Success
      | CXResult_Invalid
      | CXResult_VisitBreak

    let mapping =
      [ CXResult_Success, constant "CXResult_Success" int64_t
      ; CXResult_Invalid, constant "CXResult_Invalid" int64_t
      ; CXResult_VisitBreak, constant "CXResult_VisitBreak" int64_t
      ]


    let t : t typ =
      enum "CXResult" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXCursorAndRangeVisitorBlock = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXCursorAndRangeVisitorBlock"
  end

  module CXIdxClientFile = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIdxClientFile"
  end

  module CXIdxClientEntity = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIdxClientEntity"
  end

  module CXIdxClientContainer = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIdxClientContainer"
  end

  module CXIdxClientASTFile = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIdxClientASTFile"
  end

  module CXIdxLoc = struct
    type t

    let t : t structure typ = typedef (structure "CXIdxLoc") "CXIdxLoc"
    let ptr_data = field t "ptr_data" (array 2 (ptr void))
    let int_data = field t "int_data" uint
    let () = seal t
  end

  module CXIdxIncludedFileInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxIncludedFileInfo") "CXIdxIncludedFileInfo"


    let hash_loc = field t "hashLoc" CXIdxLoc.t
    let filename = field t "filename" string
    let file = field t "file" CXFile.t
    let is_import = field t "isImport" int
    let is_angled = field t "isAngled" int
    let is_module_import = field t "isModuleImport" int
    let () = seal t
  end

  module CXIdxImportedASTFileInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxImportedASTFileInfo") "CXIdxImportedASTFileInfo"


    let file = field t "file" CXFile.t
    let module_ = field t "module" CXModule.t
    let loc = field t "loc" CXIdxLoc.t
    let is_implicit = field t "isImplicit" int
    let () = seal t
  end

  module CXIdxEntityKind = struct
    type t =
      | CXIdxEntity_Unexposed
      | CXIdxEntity_Typedef
      | CXIdxEntity_Function
      | CXIdxEntity_Variable
      | CXIdxEntity_Field
      | CXIdxEntity_EnumConstant
      | CXIdxEntity_ObjCClass
      | CXIdxEntity_ObjCProtocol
      | CXIdxEntity_ObjCCategory
      | CXIdxEntity_ObjCInstanceMethod
      | CXIdxEntity_ObjCClassMethod
      | CXIdxEntity_ObjCProperty
      | CXIdxEntity_ObjCIvar
      | CXIdxEntity_Enum
      | CXIdxEntity_Struct
      | CXIdxEntity_Union
      | CXIdxEntity_CXXClass
      | CXIdxEntity_CXXNamespace
      | CXIdxEntity_CXXNamespaceAlias
      | CXIdxEntity_CXXStaticVariable
      | CXIdxEntity_CXXStaticMethod
      | CXIdxEntity_CXXInstanceMethod
      | CXIdxEntity_CXXConstructor
      | CXIdxEntity_CXXDestructor
      | CXIdxEntity_CXXConversionFunction
      | CXIdxEntity_CXXTypeAlias
      | CXIdxEntity_CXXInterface
      | CXIdxEntity_CXXConcept

    let mapping =
      [ CXIdxEntity_Unexposed, constant "CXIdxEntity_Unexposed" int64_t
      ; CXIdxEntity_Typedef, constant "CXIdxEntity_Typedef" int64_t
      ; CXIdxEntity_Function, constant "CXIdxEntity_Function" int64_t
      ; CXIdxEntity_Variable, constant "CXIdxEntity_Variable" int64_t
      ; CXIdxEntity_Field, constant "CXIdxEntity_Field" int64_t
      ; CXIdxEntity_EnumConstant, constant "CXIdxEntity_EnumConstant" int64_t
      ; CXIdxEntity_ObjCClass, constant "CXIdxEntity_ObjCClass" int64_t
      ; CXIdxEntity_ObjCProtocol, constant "CXIdxEntity_ObjCProtocol" int64_t
      ; CXIdxEntity_ObjCCategory, constant "CXIdxEntity_ObjCCategory" int64_t
      ; CXIdxEntity_ObjCInstanceMethod, constant "CXIdxEntity_ObjCInstanceMethod" int64_t
      ; CXIdxEntity_ObjCClassMethod, constant "CXIdxEntity_ObjCClassMethod" int64_t
      ; CXIdxEntity_ObjCProperty, constant "CXIdxEntity_ObjCProperty" int64_t
      ; CXIdxEntity_ObjCIvar, constant "CXIdxEntity_ObjCIvar" int64_t
      ; CXIdxEntity_Enum, constant "CXIdxEntity_Enum" int64_t
      ; CXIdxEntity_Struct, constant "CXIdxEntity_Struct" int64_t
      ; CXIdxEntity_Union, constant "CXIdxEntity_Union" int64_t
      ; CXIdxEntity_CXXClass, constant "CXIdxEntity_CXXClass" int64_t
      ; CXIdxEntity_CXXNamespace, constant "CXIdxEntity_CXXNamespace" int64_t
      ; CXIdxEntity_CXXNamespaceAlias, constant "CXIdxEntity_CXXNamespaceAlias" int64_t
      ; CXIdxEntity_CXXStaticVariable, constant "CXIdxEntity_CXXStaticVariable" int64_t
      ; CXIdxEntity_CXXStaticMethod, constant "CXIdxEntity_CXXStaticMethod" int64_t
      ; CXIdxEntity_CXXInstanceMethod, constant "CXIdxEntity_CXXInstanceMethod" int64_t
      ; CXIdxEntity_CXXConstructor, constant "CXIdxEntity_CXXConstructor" int64_t
      ; CXIdxEntity_CXXDestructor, constant "CXIdxEntity_CXXDestructor" int64_t
      ; ( CXIdxEntity_CXXConversionFunction
        , constant "CXIdxEntity_CXXConversionFunction" int64_t )
      ; CXIdxEntity_CXXTypeAlias, constant "CXIdxEntity_CXXTypeAlias" int64_t
      ; CXIdxEntity_CXXInterface, constant "CXIdxEntity_CXXInterface" int64_t
      ; CXIdxEntity_CXXConcept, constant "CXIdxEntity_CXXConcept" int64_t
      ]


    let t : t typ =
      enum "CXIdxEntityKind" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIdxEntityLanguage = struct
    type t =
      | CXIdxEntityLang_None
      | CXIdxEntityLang_C
      | CXIdxEntityLang_ObjC
      | CXIdxEntityLang_CXX
      | CXIdxEntityLang_Swift

    let mapping =
      [ CXIdxEntityLang_None, constant "CXIdxEntityLang_None" int64_t
      ; CXIdxEntityLang_C, constant "CXIdxEntityLang_C" int64_t
      ; CXIdxEntityLang_ObjC, constant "CXIdxEntityLang_ObjC" int64_t
      ; CXIdxEntityLang_CXX, constant "CXIdxEntityLang_CXX" int64_t
      ; CXIdxEntityLang_Swift, constant "CXIdxEntityLang_Swift" int64_t
      ]


    let t : t typ =
      enum "CXIdxEntityLanguage" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIdxEntityCXXTemplateKind = struct
    type t =
      | CXIdxEntity_NonTemplate
      | CXIdxEntity_Template
      | CXIdxEntity_TemplatePartialSpecialization
      | CXIdxEntity_TemplateSpecialization

    let mapping =
      [ CXIdxEntity_NonTemplate, constant "CXIdxEntity_NonTemplate" int64_t
      ; CXIdxEntity_Template, constant "CXIdxEntity_Template" int64_t
      ; ( CXIdxEntity_TemplatePartialSpecialization
        , constant "CXIdxEntity_TemplatePartialSpecialization" int64_t )
      ; ( CXIdxEntity_TemplateSpecialization
        , constant "CXIdxEntity_TemplateSpecialization" int64_t )
      ]


    let t : t typ =
      enum "CXIdxEntityCXXTemplateKind" ~typedef:true mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXIdxAttrKind = struct
    type t =
      | CXIdxAttr_Unexposed
      | CXIdxAttr_IBAction
      | CXIdxAttr_IBOutlet
      | CXIdxAttr_IBOutletCollection

    let mapping =
      [ CXIdxAttr_Unexposed, constant "CXIdxAttr_Unexposed" int64_t
      ; CXIdxAttr_IBAction, constant "CXIdxAttr_IBAction" int64_t
      ; CXIdxAttr_IBOutlet, constant "CXIdxAttr_IBOutlet" int64_t
      ; CXIdxAttr_IBOutletCollection, constant "CXIdxAttr_IBOutletCollection" int64_t
      ]


    let t : t typ =
      enum "CXIdxAttrKind" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIdxAttrInfo = struct
    type t

    let t : t structure typ = typedef (structure "CXIdxAttrInfo") "CXIdxAttrInfo"
    let kind = field t "kind" CXIdxAttrKind.t
    let cursor = field t "cursor" CXCursor.t
    let loc = field t "loc" CXIdxLoc.t
    let () = seal t
  end

  module CXIdxEntityInfo = struct
    type t

    let t : t structure typ = typedef (structure "CXIdxEntityInfo") "CXIdxEntityInfo"
    let kind = field t "kind" CXIdxEntityKind.t
    let template_kind = field t "templateKind" CXIdxEntityCXXTemplateKind.t
    let lang = field t "lang" CXIdxEntityLanguage.t
    let name = field t "name" string
    let usr = field t "USR" string
    let cursor = field t "cursor" CXCursor.t
    let attributes = field t "attributes" (ptr (ptr (const CXIdxAttrInfo.t)))
    let num_attributes = field t "numAttributes" uint
    let () = seal t
  end

  module CXIdxContainerInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxContainerInfo") "CXIdxContainerInfo"


    let cursor = field t "cursor" CXCursor.t
    let () = seal t
  end

  module CXIdxIBOutletCollectionAttrInfo = struct
    type t

    let t : t structure typ =
      typedef
        (structure "CXIdxIBOutletCollectionAttrInfo")
        "CXIdxIBOutletCollectionAttrInfo"


    let attr_info = field t "attrInfo" (ptr (const CXIdxAttrInfo.t))
    let objc_class = field t "objcClass" (ptr (const CXIdxEntityInfo.t))
    let class_cursor = field t "classCursor" CXCursor.t
    let class_loc = field t "classLoc" CXIdxLoc.t
    let () = seal t
  end

  module CXIdxDeclInfoFlags = struct
    type t = CXIdxDeclFlag_Skipped

    let mapping = [ CXIdxDeclFlag_Skipped, constant "CXIdxDeclFlag_Skipped" int64_t ]

    let t : t typ =
      enum "CXIdxDeclInfoFlags" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIdxDeclInfo = struct
    type t

    let t : t structure typ = typedef (structure "CXIdxDeclInfo") "CXIdxDeclInfo"
    let entity_info = field t "entityInfo" (ptr (const CXIdxEntityInfo.t))
    let cursor = field t "cursor" CXCursor.t
    let loc = field t "loc" CXIdxLoc.t

    let semantic_container =
      field t "semanticContainer" (ptr (const CXIdxContainerInfo.t))


    let lexical_container = field t "lexicalContainer" (ptr (const CXIdxContainerInfo.t))
    let is_redeclaration = field t "isRedeclaration" int
    let is_definition = field t "isDefinition" int
    let is_container = field t "isContainer" int
    let decl_as_container = field t "declAsContainer" (ptr (const CXIdxContainerInfo.t))
    let is_implicit = field t "isImplicit" int
    let attributes = field t "attributes" (ptr (ptr (const CXIdxAttrInfo.t)))
    let num_attributes = field t "numAttributes" uint
    let flags = field t "flags" uint
    let () = seal t
  end

  module CXIdxObjCContainerKind = struct
    type t =
      | CXIdxObjCContainer_ForwardRef
      | CXIdxObjCContainer_Interface
      | CXIdxObjCContainer_Implementation

    let mapping =
      [ CXIdxObjCContainer_ForwardRef, constant "CXIdxObjCContainer_ForwardRef" int64_t
      ; CXIdxObjCContainer_Interface, constant "CXIdxObjCContainer_Interface" int64_t
      ; ( CXIdxObjCContainer_Implementation
        , constant "CXIdxObjCContainer_Implementation" int64_t )
      ]


    let t : t typ =
      enum "CXIdxObjCContainerKind" ~typedef:true mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXIdxObjCContainerDeclInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCContainerDeclInfo") "CXIdxObjCContainerDeclInfo"


    let decl_info = field t "declInfo" (ptr (const CXIdxDeclInfo.t))
    let kind = field t "kind" CXIdxObjCContainerKind.t
    let () = seal t
  end

  module CXIdxBaseClassInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxBaseClassInfo") "CXIdxBaseClassInfo"


    let base = field t "base" (ptr (const CXIdxEntityInfo.t))
    let cursor = field t "cursor" CXCursor.t
    let loc = field t "loc" CXIdxLoc.t
    let () = seal t
  end

  module CXIdxObjCProtocolRefInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCProtocolRefInfo") "CXIdxObjCProtocolRefInfo"


    let protocol = field t "protocol" (ptr (const CXIdxEntityInfo.t))
    let cursor = field t "cursor" CXCursor.t
    let loc = field t "loc" CXIdxLoc.t
    let () = seal t
  end

  module CXIdxObjCProtocolRefListInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCProtocolRefListInfo") "CXIdxObjCProtocolRefListInfo"


    let protocols = field t "protocols" (ptr (ptr (const CXIdxObjCProtocolRefInfo.t)))
    let num_protocols = field t "numProtocols" uint
    let () = seal t
  end

  module CXIdxObjCInterfaceDeclInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCInterfaceDeclInfo") "CXIdxObjCInterfaceDeclInfo"


    let container_info =
      field t "containerInfo" (ptr (const CXIdxObjCContainerDeclInfo.t))


    let super_info = field t "superInfo" (ptr (const CXIdxBaseClassInfo.t))
    let protocols = field t "protocols" (ptr (const CXIdxObjCProtocolRefListInfo.t))
    let () = seal t
  end

  module CXIdxObjCCategoryDeclInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCCategoryDeclInfo") "CXIdxObjCCategoryDeclInfo"


    let container_info =
      field t "containerInfo" (ptr (const CXIdxObjCContainerDeclInfo.t))


    let objc_class = field t "objcClass" (ptr (const CXIdxEntityInfo.t))
    let class_cursor = field t "classCursor" CXCursor.t
    let class_loc = field t "classLoc" CXIdxLoc.t
    let protocols = field t "protocols" (ptr (const CXIdxObjCProtocolRefListInfo.t))
    let () = seal t
  end

  module CXIdxObjCPropertyDeclInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxObjCPropertyDeclInfo") "CXIdxObjCPropertyDeclInfo"


    let decl_info = field t "declInfo" (ptr (const CXIdxDeclInfo.t))
    let getter = field t "getter" (ptr (const CXIdxEntityInfo.t))
    let setter = field t "setter" (ptr (const CXIdxEntityInfo.t))
    let () = seal t
  end

  module CXIdxCXXClassDeclInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxCXXClassDeclInfo") "CXIdxCXXClassDeclInfo"


    let decl_info = field t "declInfo" (ptr (const CXIdxDeclInfo.t))
    let bases = field t "bases" (ptr (ptr (const CXIdxBaseClassInfo.t)))
    let num_bases = field t "numBases" uint
    let () = seal t
  end

  module CXIdxEntityRefKind = struct
    type t =
      | CXIdxEntityRef_Direct
      | CXIdxEntityRef_Implicit

    let mapping =
      [ CXIdxEntityRef_Direct, constant "CXIdxEntityRef_Direct" int64_t
      ; CXIdxEntityRef_Implicit, constant "CXIdxEntityRef_Implicit" int64_t
      ]


    let t : t typ =
      enum "CXIdxEntityRefKind" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXSymbolRole = struct
    type t =
      | CXSymbolRole_None
      | CXSymbolRole_Declaration
      | CXSymbolRole_Definition
      | CXSymbolRole_Reference
      | CXSymbolRole_Read
      | CXSymbolRole_Write
      | CXSymbolRole_Call
      | CXSymbolRole_Dynamic
      | CXSymbolRole_AddressOf
      | CXSymbolRole_Implicit

    let mapping =
      [ CXSymbolRole_None, constant "CXSymbolRole_None" int64_t
      ; CXSymbolRole_Declaration, constant "CXSymbolRole_Declaration" int64_t
      ; CXSymbolRole_Definition, constant "CXSymbolRole_Definition" int64_t
      ; CXSymbolRole_Reference, constant "CXSymbolRole_Reference" int64_t
      ; CXSymbolRole_Read, constant "CXSymbolRole_Read" int64_t
      ; CXSymbolRole_Write, constant "CXSymbolRole_Write" int64_t
      ; CXSymbolRole_Call, constant "CXSymbolRole_Call" int64_t
      ; CXSymbolRole_Dynamic, constant "CXSymbolRole_Dynamic" int64_t
      ; CXSymbolRole_AddressOf, constant "CXSymbolRole_AddressOf" int64_t
      ; CXSymbolRole_Implicit, constant "CXSymbolRole_Implicit" int64_t
      ]


    let t : t typ =
      enum "CXSymbolRole" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXIdxEntityRefInfo = struct
    type t

    let t : t structure typ =
      typedef (structure "CXIdxEntityRefInfo") "CXIdxEntityRefInfo"


    let kind = field t "kind" CXIdxEntityRefKind.t
    let cursor = field t "cursor" CXCursor.t
    let loc = field t "loc" CXIdxLoc.t
    let referenced_entity = field t "referencedEntity" (ptr (const CXIdxEntityInfo.t))
    let parent_entity = field t "parentEntity" (ptr (const CXIdxEntityInfo.t))
    let container = field t "container" (ptr (const CXIdxContainerInfo.t))
    let role = field t "role" CXSymbolRole.t
    let () = seal t
  end

  module CXDiagnosticSet = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXDiagnosticSet"
  end

  module IndexerCallbacks = struct
    type t

    let t : t structure typ = typedef (structure "IndexerCallbacks") "IndexerCallbacks"

    let abort_query =
      field t "abortQuery" (static_funptr (CXClientData.t @-> ptr void @-> returning int))


    let diagnostic =
      field
        t
        "diagnostic"
        (static_funptr
           (CXClientData.t @-> CXDiagnosticSet.t @-> ptr void @-> returning void))


    let entered_main_file =
      field
        t
        "enteredMainFile"
        (static_funptr
           (CXClientData.t @-> CXFile.t @-> ptr void @-> returning CXIdxClientFile.t))


    let pp_included_file =
      field
        t
        "ppIncludedFile"
        (static_funptr
           (CXClientData.t
            @-> ptr (const CXIdxIncludedFileInfo.t)
            @-> returning CXIdxClientFile.t))


    let imported_astfile =
      field
        t
        "importedASTFile"
        (static_funptr
           (CXClientData.t
            @-> ptr (const CXIdxImportedASTFileInfo.t)
            @-> returning CXIdxClientASTFile.t))


    let started_translation_unit =
      field
        t
        "startedTranslationUnit"
        (static_funptr (CXClientData.t @-> ptr void @-> returning CXIdxClientContainer.t))


    let index_declaration =
      field
        t
        "indexDeclaration"
        (static_funptr
           (CXClientData.t @-> ptr (const CXIdxDeclInfo.t) @-> returning void))


    let index_entity_reference =
      field
        t
        "indexEntityReference"
        (static_funptr
           (CXClientData.t @-> ptr (const CXIdxEntityRefInfo.t) @-> returning void))


    let () = seal t
  end

  module CXIndexAction = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXIndexAction"
  end

  module CXIndexOptFlags = struct
    type t =
      | CXIndexOpt_None
      | CXIndexOpt_SuppressRedundantRefs
      | CXIndexOpt_IndexFunctionLocalSymbols
      | CXIndexOpt_IndexImplicitTemplateInstantiations
      | CXIndexOpt_SuppressWarnings
      | CXIndexOpt_SkipParsedBodiesInSession

    let mapping =
      [ CXIndexOpt_None, constant "CXIndexOpt_None" int64_t
      ; ( CXIndexOpt_SuppressRedundantRefs
        , constant "CXIndexOpt_SuppressRedundantRefs" int64_t )
      ; ( CXIndexOpt_IndexFunctionLocalSymbols
        , constant "CXIndexOpt_IndexFunctionLocalSymbols" int64_t )
      ; ( CXIndexOpt_IndexImplicitTemplateInstantiations
        , constant "CXIndexOpt_IndexImplicitTemplateInstantiations" int64_t )
      ; CXIndexOpt_SuppressWarnings, constant "CXIndexOpt_SuppressWarnings" int64_t
      ; ( CXIndexOpt_SkipParsedBodiesInSession
        , constant "CXIndexOpt_SkipParsedBodiesInSession" int64_t )
      ]


    let t : t typ =
      enum "CXIndexOptFlags" ~typedef:true mapping ~unexpected:(fun _ -> assert false)
  end

  module CXFieldVisitor = struct
    type t = CXCursor.t structure ptr -> CXClientData.t -> CXVisitorResult.t

    let t : t static_funptr typ =
      typedef
        (static_funptr
           (ptr (const CXCursor.t) @-> CXClientData.t @-> returning CXVisitorResult.t))
        "CXFieldVisitor"
  end

  module CXBinaryOperatorKind = struct
    type t =
      | CXBinaryOperator_Invalid
      | CXBinaryOperator_PtrMemD
      | CXBinaryOperator_PtrMemI
      | CXBinaryOperator_Mul
      | CXBinaryOperator_Div
      | CXBinaryOperator_Rem
      | CXBinaryOperator_Add
      | CXBinaryOperator_Sub
      | CXBinaryOperator_Shl
      | CXBinaryOperator_Shr
      | CXBinaryOperator_Cmp
      | CXBinaryOperator_LT
      | CXBinaryOperator_GT
      | CXBinaryOperator_LE
      | CXBinaryOperator_GE
      | CXBinaryOperator_EQ
      | CXBinaryOperator_NE
      | CXBinaryOperator_And
      | CXBinaryOperator_Xor
      | CXBinaryOperator_Or
      | CXBinaryOperator_LAnd
      | CXBinaryOperator_LOr
      | CXBinaryOperator_Assign
      | CXBinaryOperator_MulAssign
      | CXBinaryOperator_DivAssign
      | CXBinaryOperator_RemAssign
      | CXBinaryOperator_AddAssign
      | CXBinaryOperator_SubAssign
      | CXBinaryOperator_ShlAssign
      | CXBinaryOperator_ShrAssign
      | CXBinaryOperator_AndAssign
      | CXBinaryOperator_XorAssign
      | CXBinaryOperator_OrAssign
      | CXBinaryOperator_Comma

    let mapping =
      [ CXBinaryOperator_Invalid, constant "CXBinaryOperator_Invalid" int64_t
      ; CXBinaryOperator_PtrMemD, constant "CXBinaryOperator_PtrMemD" int64_t
      ; CXBinaryOperator_PtrMemI, constant "CXBinaryOperator_PtrMemI" int64_t
      ; CXBinaryOperator_Mul, constant "CXBinaryOperator_Mul" int64_t
      ; CXBinaryOperator_Div, constant "CXBinaryOperator_Div" int64_t
      ; CXBinaryOperator_Rem, constant "CXBinaryOperator_Rem" int64_t
      ; CXBinaryOperator_Add, constant "CXBinaryOperator_Add" int64_t
      ; CXBinaryOperator_Sub, constant "CXBinaryOperator_Sub" int64_t
      ; CXBinaryOperator_Shl, constant "CXBinaryOperator_Shl" int64_t
      ; CXBinaryOperator_Shr, constant "CXBinaryOperator_Shr" int64_t
      ; CXBinaryOperator_Cmp, constant "CXBinaryOperator_Cmp" int64_t
      ; CXBinaryOperator_LT, constant "CXBinaryOperator_LT" int64_t
      ; CXBinaryOperator_GT, constant "CXBinaryOperator_GT" int64_t
      ; CXBinaryOperator_LE, constant "CXBinaryOperator_LE" int64_t
      ; CXBinaryOperator_GE, constant "CXBinaryOperator_GE" int64_t
      ; CXBinaryOperator_EQ, constant "CXBinaryOperator_EQ" int64_t
      ; CXBinaryOperator_NE, constant "CXBinaryOperator_NE" int64_t
      ; CXBinaryOperator_And, constant "CXBinaryOperator_And" int64_t
      ; CXBinaryOperator_Xor, constant "CXBinaryOperator_Xor" int64_t
      ; CXBinaryOperator_Or, constant "CXBinaryOperator_Or" int64_t
      ; CXBinaryOperator_LAnd, constant "CXBinaryOperator_LAnd" int64_t
      ; CXBinaryOperator_LOr, constant "CXBinaryOperator_LOr" int64_t
      ; CXBinaryOperator_Assign, constant "CXBinaryOperator_Assign" int64_t
      ; CXBinaryOperator_MulAssign, constant "CXBinaryOperator_MulAssign" int64_t
      ; CXBinaryOperator_DivAssign, constant "CXBinaryOperator_DivAssign" int64_t
      ; CXBinaryOperator_RemAssign, constant "CXBinaryOperator_RemAssign" int64_t
      ; CXBinaryOperator_AddAssign, constant "CXBinaryOperator_AddAssign" int64_t
      ; CXBinaryOperator_SubAssign, constant "CXBinaryOperator_SubAssign" int64_t
      ; CXBinaryOperator_ShlAssign, constant "CXBinaryOperator_ShlAssign" int64_t
      ; CXBinaryOperator_ShrAssign, constant "CXBinaryOperator_ShrAssign" int64_t
      ; CXBinaryOperator_AndAssign, constant "CXBinaryOperator_AndAssign" int64_t
      ; CXBinaryOperator_XorAssign, constant "CXBinaryOperator_XorAssign" int64_t
      ; CXBinaryOperator_OrAssign, constant "CXBinaryOperator_OrAssign" int64_t
      ; CXBinaryOperator_Comma, constant "CXBinaryOperator_Comma" int64_t
      ]


    let t : t typ =
      enum "CXBinaryOperatorKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXUnaryOperatorKind = struct
    type t =
      | CXUnaryOperator_Invalid
      | CXUnaryOperator_PostInc
      | CXUnaryOperator_PostDec
      | CXUnaryOperator_PreInc
      | CXUnaryOperator_PreDec
      | CXUnaryOperator_AddrOf
      | CXUnaryOperator_Deref
      | CXUnaryOperator_Plus
      | CXUnaryOperator_Minus
      | CXUnaryOperator_Not
      | CXUnaryOperator_LNot
      | CXUnaryOperator_Real
      | CXUnaryOperator_Imag
      | CXUnaryOperator_Extension
      | CXUnaryOperator_Coawait

    let mapping =
      [ CXUnaryOperator_Invalid, constant "CXUnaryOperator_Invalid" int64_t
      ; CXUnaryOperator_PostInc, constant "CXUnaryOperator_PostInc" int64_t
      ; CXUnaryOperator_PostDec, constant "CXUnaryOperator_PostDec" int64_t
      ; CXUnaryOperator_PreInc, constant "CXUnaryOperator_PreInc" int64_t
      ; CXUnaryOperator_PreDec, constant "CXUnaryOperator_PreDec" int64_t
      ; CXUnaryOperator_AddrOf, constant "CXUnaryOperator_AddrOf" int64_t
      ; CXUnaryOperator_Deref, constant "CXUnaryOperator_Deref" int64_t
      ; CXUnaryOperator_Plus, constant "CXUnaryOperator_Plus" int64_t
      ; CXUnaryOperator_Minus, constant "CXUnaryOperator_Minus" int64_t
      ; CXUnaryOperator_Not, constant "CXUnaryOperator_Not" int64_t
      ; CXUnaryOperator_LNot, constant "CXUnaryOperator_LNot" int64_t
      ; CXUnaryOperator_Real, constant "CXUnaryOperator_Real" int64_t
      ; CXUnaryOperator_Imag, constant "CXUnaryOperator_Imag" int64_t
      ; CXUnaryOperator_Extension, constant "CXUnaryOperator_Extension" int64_t
      ; CXUnaryOperator_Coawait, constant "CXUnaryOperator_Coawait" int64_t
      ]


    let t : t typ =
      enum "CXUnaryOperatorKind" ~typedef:false mapping ~unexpected:(fun _ ->
        assert false)
  end

  module CXSourceRangeList = struct
    type t

    let t : t structure typ = typedef (structure "CXSourceRangeList") "CXSourceRangeList"
    let count = field t "count" uint
    let ranges = field t "ranges" (ptr CXSourceRange.t)
    let () = seal t
  end

  module CXDiagnostic = struct
    type t = unit ptr

    let t : t typ = typedef (ptr void) "CXDiagnostic"
  end

  module CXErrorCode = struct
    type t =
      | CXError_Success
      | CXError_Failure
      | CXError_Crashed
      | CXError_InvalidArguments
      | CXError_ASTReadError

    let mapping =
      [ CXError_Success, constant "CXError_Success" int64_t
      ; CXError_Failure, constant "CXError_Failure" int64_t
      ; CXError_Crashed, constant "CXError_Crashed" int64_t
      ; CXError_InvalidArguments, constant "CXError_InvalidArguments" int64_t
      ; CXError_ASTReadError, constant "CXError_ASTReadError" int64_t
      ]


    let t : t typ =
      enum "CXErrorCode" ~typedef:false mapping ~unexpected:(fun _ -> assert false)
  end

  module CXStringSet = struct
    type t

    let t : t structure typ = typedef (structure "CXStringSet") "CXStringSet"
    let strings = field t "Strings" (ptr CXString.t)
    let count = field t "Count" uint
    let () = seal t
  end
end

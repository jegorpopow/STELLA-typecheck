{-# LANGUAGE TupleSections #-}

module Syntax.TypeChecker where

import Cmm (Section)
import Control.Monad (unless, void, when)
import Data.Either (fromLeft, fromRight)
import Data.List (intercalate, nub, (\\))
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import Debug.Trace (trace, traceM)
import Language.Haskell.TH (Con)
import Syntax.Abs
  ( Binding (ABinding),
    Decl (DeclExceptionType, DeclExceptionVariant, DeclFun),
    Expr
      ( Abstraction,
        Application,
        Assign,
        ConsList,
        ConstFalse,
        ConstInt,
        ConstMemory,
        ConstTrue,
        ConstUnit,
        Deref,
        DotRecord,
        DotTuple,
        Fix,
        Head,
        If,
        Inl,
        Inr,
        IsEmpty,
        IsZero,
        Let,
        LetRec,
        List,
        Match,
        NatRec,
        Panic,
        Record,
        Ref,
        Sequence,
        Succ,
        Tail,
        Throw,
        TryCastAs,
        TryCatch,
        TryWith,
        Tuple,
        TypeAsc,
        Var,
        Variant
      ),
    ExprData (NoExprData, SomeExprData),
    Extension (AnExtension),
    ExtensionName (ExtensionName),
    LabelledPattern (ALabelledPattern),
    MatchCase (AMatchCase),
    OptionalTyping (NoTyping, SomeTyping),
    ParamDecl (AParamDecl),
    Pattern (PatternAsc, PatternCastAs, PatternCons, PatternFalse, PatternInl, PatternInr, PatternInt, PatternList, PatternRecord, PatternSucc, PatternTrue, PatternTuple, PatternUnit, PatternVar, PatternVariant),
    PatternBinding (..),
    PatternData (NoPatternData, SomePatternData),
    Program (..),
    RecordFieldType (ARecordFieldType),
    ReturnType (SomeReturnType),
    StellaIdent (StellaIdent),
    Type
      ( TypeBool,
        TypeBottom,
        TypeFun,
        TypeList,
        TypeNat,
        TypeRecord,
        TypeRef,
        TypeSum,
        TypeTop,
        TypeTuple,
        TypeUnit,
        TypeVariant
      ),
    VariantFieldType (AVariantFieldType),
  )
import Syntax.Print (printTree)

-- Type checker error type
-- Every type checking action can produce at most one type checker error
data TypeCheckerError
  = NoMain
  | UndefinedVariable StellaIdent
  | UnexpectedType {expr :: Expr, expected :: Type, infered :: Type}
  | UnexpectedSubtype {expr :: Expr, expected :: Type, infered :: Type}
  | UnexpectedSubtypeNoExpr {expected :: Type, infered :: Type}
  | NotAFunction Expr
  | NotATuple Expr
  | NotARecord Expr
  | NotAList Expr
  | NotAReference Expr
  | ConflicitingExceptionDeclarations
  | UnexpectedTupleLength Int Int Type Expr
  | DuplicateVariablePattern StellaIdent
  | DuplicateVariableLet StellaIdent
  | DuplicateRecordTypeFields StellaIdent Type
  | DuplicateVariantTypeMembers StellaIdent Type
  | DuplicateRecordFields StellaIdent Expr
  | DuplicateFunctionDeclaration StellaIdent
  | DuplicateFunctionParameterName StellaIdent
  | DuplicateRecordPattern StellaIdent Pattern
  | DuplicateExceptionType
  | DuplicateExceptionVariant StellaIdent
  | AmbigousSumType
  | AmbigousPanicType Expr
  | AmbigousThrowType Expr
  | AmbigousReferenceType Expr
  | AmbigousVariantType
  | AmbigousPatternType Pattern
  | AmbigousListType
  | UnexpectedRecordField Type StellaIdent
  | UnexpectedTypeForParameter [Type] [Type]
  | UnsupportedExpression Expr
  | UnsupportedDecl Decl
  | UnsupportedPattern Pattern
  | UnsupportedConstruction String
  | MissingFields [StellaIdent] [StellaIdent] Expr Type
  | UnexpectedFields [StellaIdent] [StellaIdent] Expr Type
  | EmptyMatch
  | UnexpectedLambda Expr Type
  | UnexpectedTuple Expr Type
  | UnexpectedRecord Expr Type
  | UnexpectedInjection Expr Type
  | UnexpectedReference Expr Type
  | UnexpectedMemoryAddress Expr Type
  | UnexpectedList Expr Type
  | UnexpectedVariant Expr Type
  | UnexpectedPatternForType Pattern Type
  | MismatchedArgumentsNumber Int Int Expr
  | UnexpectedNonNullaryPattern StellaIdent Pattern Type
  | UnexpectedNullaryPattern StellaIdent Pattern Type
  | UnexpectedArgumentsNumberInLambda Int Int Expr
  | UnexpectedDataForNullaryType StellaIdent Expr Type
  | MissingDataForMember StellaIdent Expr Type
  | UnexpectedVariantType StellaIdent Type
  | TupleIndexOutOfBounds Integer Type
  | WrongArityMain Int
  | ExceptionTypeNotDeclared
  | IllegalExceptionTypeDeclaration
  | IllegalExceptionVariantDeclaration
  | NonexhaustivePatternMatching [Pattern] Expr Type
  deriving (Eq, Ord, Read)

instance Show TypeCheckerError where
  show err = case err of
    NoMain ->
      "ERROR_MISSING_MAIN:\n  program has no main function"
    WrongArityMain n ->
      "ERROR_INCORRECT_ARITY_OF_MAIN\n  main have " ++ show n ++ " arguments"
    DuplicateFunctionDeclaration name -> "ERROR_DUPLICATE_FUNCTION_DECLARATION:\n  Function " ++ show name ++ " declaraded several times"
    DuplicateRecordPattern name pattern -> "ERROR_DUPLICATE_RECORD_PATTERN_FIELDS:\n  Pattern " ++ printTree pattern ++ " contains multiple occurences of label " ++ printTree name
    UndefinedVariable ident ->
      "ERROR_UNDEFINED_VARIABLE:\n  undefined variable " ++ show ident
    UnexpectedType expr expected inferred ->
      "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION:\n"
        ++ "  expected type:\n    "
        ++ printTree expected
        ++ "\n"
        ++ "  but got type:\n    "
        ++ printTree inferred
        ++ "\n"
        ++ "  for expression:\n    "
        ++ printTree expr
    UnexpectedSubtype expr expected inferred ->
      "ERROR_UNEXPECTED_SUBTYPE:\n"
        ++ "  expected (sub)type:\n    "
        ++ printTree expected
        ++ "\n"
        ++ "  but got type:\n    "
        ++ printTree inferred
        ++ "\n"
        ++ "  for expression:\n    "
        ++ printTree expr
    UnexpectedSubtypeNoExpr expected inferred ->
      "ERROR_UNEXPECTED_SUBTYPE:\n"
        ++ "  expected (sub)type:\n    "
        ++ printTree expected
        ++ "\n"
        ++ "  but got type:\n    "
        ++ printTree inferred
        ++ "\n"
    ExceptionTypeNotDeclared ->
      "ERROR_EXCEPTION_TYPE_NOT_DECLARED:\n  Exception thrown without preceding specification of excpetion type"
    AmbigousPatternType pat ->
      "ERROR_AMBIGUOUS_PATTERN_TYPE:\n  Can not infer type for pattern: " ++ printTree pat
    AmbigousReferenceType expr ->
      "ERROR_AMBIGUOUS_REFERENCE_TYPE:\n  Can not infer type for constant memory address: " ++ printTree expr
    AmbigousPanicType expr ->
      "ERROR_AMBIGUOUS_PANIC_TYPE:\n  Can not infer type for error-throwing expression: " ++ printTree expr
    AmbigousThrowType expr ->
      "ERROR_AMBIGUOUS_THROW_TYPE:\n  Can not infer type for error-throwing expression: " ++ printTree expr
    NotAFunction expr ->
      "ERROR_NOT_A_FUNCTION:\n  expression is not a function: " ++ printTree expr
    NotAReference expr ->
      "ERROR_NOT_A_REFERENCE:\n  expression is not a reference: " ++ printTree expr
    NotATuple expr ->
      "ERROR_NOT_A_TUPLE:\n  expression is not a tuple: " ++ printTree expr
    NotARecord expr ->
      "ERROR_NOT_A_RECORD:\n  expression is not a record: " ++ printTree expr
    TupleIndexOutOfBounds n t ->
      "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS:\n  accessing element ." ++ show n ++ " of type " ++ printTree t
    NotAList expr ->
      "ERROR_NOT_A_LIST:\n  expression is not a list: " ++ printTree expr
    UnexpectedLambda expr t ->
      "ERROR_UNEXPECTED_LAMBDA:\n  expected non‑function type but got lambda: " ++ printTree expr
    UnexpectedTypeForParameter expected actual ->
      "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER:\n"
        ++ "  expected parameter types: "
        ++ printTree expected
        ++ "\n"
        ++ "  but got: "
        ++ printTree actual
    UnexpectedTuple expr t ->
      "ERROR_UNEXPECTED_TUPLE:\n  expected non‑tuple type " ++ printTree t ++ " but got tuple: " ++ printTree expr
    UnexpectedRecord expr t ->
      "ERROR_UNEXPECTED_RECORD:\n  expected non‑record type " ++ printTree t ++ "  but got record: " ++ show expr
    UnexpectedReference expr t ->
      "ERROR_UNEXPECTED_REFERENCE:\n  expected non‑reference type " ++ printTree t ++ "  but got reference: " ++ show expr
    UnexpectedMemoryAddress expr t ->
      "ERROR_UNEXPECTED_MEMORY_ADDRESS:\n  expected non‑refernce type " ++ printTree t ++ "  but got memory address: " ++ printTree expr
    UnexpectedVariant expr t ->
      "ERROR_UNEXPECTED_VARIANT:\n  expected non‑variant type " ++ printTree t ++ "  but got variant: " ++ printTree expr
    UnexpectedDataForNullaryType label expr t ->
      "ERROR_UNEXPECTED_DATA_FOR_NULLARY_LABEL:\n  providing expession " ++ printTree expr ++ " with not null for label " ++ show label ++ " for type" ++ printTree t
    MissingDataForMember label expr t ->
      "ERROR_MISSING_DATA_FOR_LABEL:\n  providing expession " ++ printTree expr ++ " with null for label " ++ show label ++ " for type " ++ printTree t
    UnexpectedList expr t ->
      "ERROR_UNEXPECTED_LIST:\n  expected non‑list type " ++ printTree t ++ "  but got list: " ++ printTree expr
    UnexpectedInjection expr t ->
      "ERROR_UNEXPECTED_INJECTION:\n  expected non‑sum type" ++ printTree t ++ "  but got injection: " ++ printTree expr
    MissingFields expected actual e t ->
      let missing = expected \\ actual
       in "ERROR_MISSING_RECORD_FIELDS:\n"
            ++ "  missing fields: "
            ++ intercalate ", " (map printTree missing)
            ++ "\n"
            ++ "  expected fields: "
            ++ intercalate ", " (map printTree expected)
            ++ "\n"
            ++ "  actual fields: "
            ++ intercalate ", " (map printTree actual)
            ++ "\n"
            ++ "  in value"
            ++ printTree e
            ++ "  of type"
            ++ printTree t
    UnexpectedNonNullaryPattern label pat t ->
      "ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN:\n  expected non-nullary pattern fot label " ++ show label ++ " in pattern" ++ printTree pat ++ " for type " ++ printTree t
    UnexpectedNullaryPattern label pat t ->
      "ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN:\n  expected non-nullary pattern fot label " ++ show label ++ " in pattern" ++ printTree pat ++ " for type " ++ printTree t
    UnexpectedFields expected actual e t ->
      let unexpected = actual \\ expected
       in "ERROR_UNEXPECTED_RECORD_FIELDS:\n"
            ++ "  unexpected fields: "
            ++ intercalate ", " (map printTree unexpected)
            ++ "\n"
            ++ "  expected fields: "
            ++ intercalate ", " (map printTree expected)
            ++ "\n"
            ++ "  actual fields: "
            ++ intercalate ", " (map printTree actual)
            ++ "\n"
            ++ "  in value"
            ++ printTree e
            ++ "  of type"
            ++ printTree t
    UnexpectedRecordField recType field ->
      "ERROR_UNEXPECTED_FIELD_ACCESS:\n"
        ++ "  field "
        ++ printTree field
        ++ " not found in record type "
        ++ printTree recType
    UnexpectedVariantType label varType ->
      "ERROR_UNEXPECTED_VARIANT_LABEL:\n"
        ++ "  label "
        ++ printTree label
        ++ " not found in variant type "
        ++ printTree varType
    UnexpectedTupleLength expectedLength actualLength t e ->
      "ERROR_UNEXPECTED_TUPLE_LENGTH:\n"
        ++ "  expected tuple type: "
        ++ printTree t
        ++ " of length "
        ++ show expectedLength
        ++ "\n"
        ++ "  , but got tuple "
        ++ printTree e
        ++ "  of length "
        ++ show actualLength
    ConflicitingExceptionDeclarations ->
      "ERROR_CONFLICTING_EXCEPTION_DECLARATIONS: several declarations of exception type/variant found"
    DuplicateExceptionType ->
      "ERROR_DUPLICATE_EXCEPTION_TYPE: several declarations of exception type found"
    DuplicateExceptionVariant name ->
      "ERROR_DUPLICATE_EXCEPTION_VARIANT: several declarations of exception label " ++ printTree name ++ " found"
    DuplicateVariableLet name ->
      "ERROR_DUPLICATE_LET_BINDING:\n  duplicate variable in let:\n" ++ printTree name
    DuplicateVariablePattern name ->
      "ERROR_DUPLICATE_VARIABLE_PATTERN:\n  duplicate variable in pattern:\n" ++ printTree name
    DuplicateFunctionParameterName name ->
      "ERROR_DUPLICATE_FUNCTION_PARAMETER:\n  duplicate parameter name:\n" ++ printTree name
    DuplicateRecordFields field e ->
      "ERROR_DUPLICATE_RECORD_FIELDS:\n  duplicate field: " ++ show field ++ " in record expression " ++ printTree e
    DuplicateRecordTypeFields field t ->
      "ERROR_DUPLICATE_RECORD_TYPE_FIELDS:\n  duplicate field: " ++ show field ++ " in record type " ++ printTree t
    DuplicateVariantTypeMembers field t ->
      "ERROR_DUPLICATE_VARIANT_TYPE_FIELDS:\n  duplicate field: " ++ show field ++ " in variant type " ++ printTree t
    AmbigousSumType ->
      "ERROR_AMBIGUOUS_SUM_TYPE:\n  ambiguous sum type (cannot determine type of injection)"
    AmbigousVariantType ->
      "ERROR_AMBIGUOUS_VARIANT_TYPE:\n  ambiguous variant type (cannot determine type of variant)"
    AmbigousListType ->
      "ERROR_AMBIGUOUS_LIST_TYPE:\n  ambiguous list type (cannot determine type of list)"
    UnsupportedExpression expr ->
      "ERROR_UNSUPPORTED_EXPRESSION:\n  unsupported expression: " ++ printTree expr
    UnsupportedDecl decl ->
      "ERROR_UNSUPPORTED_DECL:\n  unsupported declaration: " ++ printTree decl
    UnsupportedPattern pat ->
      "ERROR_UNSUPPORTED_PATTERN:\n  unsupported pattern: " ++ printTree pat
    UnsupportedConstruction s ->
      "ERROR_UNSUPPORTED_CONSTRUCTION:\n  unsupported construction: " ++ s
    EmptyMatch ->
      "ERROR_ILLEGAL_EMPTY_MATCHING:\n  match expression with no branches"
    UnexpectedPatternForType pat ty ->
      "ERROR_UNEXPECTED_PATTERN_FOR_TYPE:\n"
        ++ "  pattern "
        ++ printTree pat
        ++ " does not match type "
        ++ printTree ty
    MismatchedArgumentsNumber expected actual expr ->
      "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS:\n"
        ++ "  expected "
        ++ show expected
        ++ " arguments but got "
        ++ show actual
        ++ " in application:\n    "
        ++ printTree expr
    UnexpectedArgumentsNumberInLambda expected actual expr ->
      "ERROR_UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA:\n"
        ++ "  lambda expects "
        ++ show expected
        ++ " parameters but got "
        ++ show actual
        ++ " arguments in body?\n    "
        ++ printTree expr
    NonexhaustivePatternMatching patterns e t ->
      "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS:\n"
        ++ "  Set  of patterns\n"
        ++ "  "
        ++ intercalate ", " (map printTree patterns)
        ++ "  does not cover all the values of expession "
        ++ "  "
        ++ printTree e
        ++ " of type "
        ++ printTree t
    IllegalExceptionTypeDeclaration -> "ERROR_ILLEGAL_LOCAL_EXCEPTION_TYPE:\n"
    IllegalExceptionVariantDeclaration -> "ERROR_ILLEGAL_LOCAL_EXCEPTION_VARIANT:\n"

type TypeCheckerResult t = Either TypeCheckerError t

type Bindings = [(StellaIdent, Type)]

data ExceptionType
  = NoException
  | KnownException Type
  | OpenException [VariantFieldType]

data Context = Context
  { ctxBindings :: Bindings,
    ctxException :: ExceptionType,
    ctxAmbigiousisBottom :: Bool,
    ctxSubtyping :: Bool
  }

contextEmpty :: ExceptionType -> Bool -> Bool -> Context
contextEmpty = Context []

contextLookup :: Context -> StellaIdent -> TypeCheckerResult Type
contextLookup ctx name = case lookup name (ctxBindings ctx) of
  Just t -> return t
  Nothing -> Left $ UndefinedVariable name

-- TODO: optics ???
contextExtend :: Context -> Bindings -> Context
contextExtend (Context bindings exc ab s) bindings' =
  Context (bindings' ++ bindings) exc ab s

contextCurrentException :: Context -> TypeCheckerResult Type
contextCurrentException (Context _ e _ _) = case e of
  NoException -> Left $ ExceptionTypeNotDeclared
  KnownException t -> return t
  OpenException variants -> return $ TypeVariant variants

contextIsSuitable :: Context -> Type -> Type -> Bool
contextIsSuitable ctx = if ctxSubtyping ctx then (<:) else (==)

contextGenericError :: Context -> Expr -> Type -> Type -> TypeCheckerError
contextGenericError ctx e infered expected =
  if ctxSubtyping ctx
    then UnexpectedSubtype e expected infered
    else UnexpectedType e expected infered

contextCheckIsSuitable :: Context -> Expr -> Type -> Type -> TypeCheckerResult ()
contextCheckIsSuitable ctx e infered target =
  unless (contextIsSuitable ctx infered target) (Left $ contextGenericError ctx e infered target)

contextEnsureIsSuitable :: Context -> Expr -> Type -> TypeCheckerResult ()
contextEnsureIsSuitable ctx e target = case ensure ctx e target of
  Right () -> Right ()
  Left _ -> do
    infered <- infer ctx e
    contextCheckIsSuitable ctx e infered target

type FunctionSignature = (StellaIdent, Type)

extractFunctionSignature :: Decl -> TypeCheckerResult FunctionSignature
extractFunctionSignature (DeclFun _ name params (SomeReturnType return_type) _ _ _) = Right (name, TypeFun [t | (AParamDecl _ t) <- params] return_type)
extractFunctionSignature decl = Left $ UnsupportedDecl decl

collectFuncDecls :: [Decl] -> TypeCheckerResult Bindings
collectFuncDecls decls = sequence $ extractFunctionSignature <$> filter isFunctionDecl decls
  where
    isFunctionDecl DeclFun {} = True
    isFunctionDecl _ = False

typeCheckFunction :: Bool -> Context -> Decl -> TypeCheckerResult ()
typeCheckFunction _ ctx (DeclFun _ name params (SomeReturnType return_type) _ nested body) = do
  ctxExtension <- paramsToContext params
  let extendedCtx = contextExtend ctx ctxExtension
  typecheckFunctions True extendedCtx nested
  nestedFunctionCtx <- collectFuncDecls nested
  let bodyContext = contextExtend extendedCtx nestedFunctionCtx
  validate'n'ensure'ctx bodyContext body return_type
typeCheckFunction local _ (DeclExceptionType _) =
  when local $ Left IllegalExceptionTypeDeclaration
typeCheckFunction local _ (DeclExceptionVariant _ _) =
  when local $ Left IllegalExceptionVariantDeclaration
typeCheckFunction local _ decl = Left $ UnsupportedDecl decl

typecheckFunctions :: Bool -> Context -> [Decl] -> TypeCheckerResult ()
typecheckFunctions local ctx decls = do
  signatures <- collectFuncDecls decls
  functionsCtx <- ensureNoDuplicates signatures
  let extendedCtx = contextExtend ctx functionsCtx
  sequence_ $ typeCheckFunction local extendedCtx <$> decls
  where
    ensureNoDuplicates :: [FunctionSignature] -> TypeCheckerResult Bindings
    ensureNoDuplicates signatures =
      case duplicateIn [name | (name, _) <- signatures] of
        Nothing -> Right signatures
        Just duplicated -> Left $ DuplicateFunctionDeclaration duplicated

data TCCfg = Cfg
  { cfgBotInfer :: Bool,
    cfgSubTypes :: Bool
  }

extensionNames :: [Extension] -> [String]
extensionNames ext_decls =
  let extensions = [name | (AnExtension exts) <- ext_decls, (ExtensionName name) <- exts]
   in {- trace (show extensions) -} extensions

-- returns ExceptionType for a program. As we know,
-- open variant exception is just a fancy way for declaring
-- simple (closed variant exception), since it does not work in baseline
getExceptionType :: Program -> TypeCheckerResult ExceptionType
getExceptionType (AProgram _ _ decls)
  | null exceptionTypeDecls && null exceptionVariantDecls =
    return NoException
  | null exceptionVariantDecls && length exceptionTypeDecls /= 1 =
    Left $ DuplicateExceptionType
  | null exceptionVariantDecls =
    let (DeclExceptionType t) = head exceptionTypeDecls
     in return $ KnownException t
  | null exceptionTypeDecls =
    case duplicateIn [name | (DeclExceptionVariant name _) <- exceptionVariantDecls] of
      Just name -> Left $ DuplicateExceptionVariant name
      Nothing -> return $ OpenException [AVariantFieldType name (SomeTyping t) | (DeclExceptionVariant name t) <- exceptionVariantDecls]
  | otherwise = Left $ ConflicitingExceptionDeclarations
  where
    isExceptionTypeDeclaration (DeclExceptionType _) = True
    isExceptionTypeDeclaration _ = False
    isExceptionVariantDeclaration (DeclExceptionVariant _ _) = True
    isExceptionVariantDeclaration _ = False
    exceptionTypeDecls = filter isExceptionTypeDeclaration decls
    exceptionVariantDecls = filter isExceptionVariantDeclaration decls

-- Entry point of type checker: checks the whole program
typeCheck :: Program -> TypeCheckerResult ()
typeCheck program@(AProgram _ extensions decls) = do
  let exNames = extensionNames extensions
  let ambitiosIsBottom = "#ambiguous-type-as-bottom" `elem` exNames
  let subtypingAllowed = "#structural-subtyping" `elem` exNames
  -- traceM ("Ambitious to bottom: " ++ show ambitiosIsBottom ++ ", Subtyping = " ++ show subtypingAllowed)
  exceptionType <- getExceptionType program
  collectFuncDecls decls >>= ensureMainValid
  typecheckFunctions False (contextEmpty exceptionType ambitiosIsBottom subtypingAllowed) decls
  where
    ensureMainValid :: [FunctionSignature] -> TypeCheckerResult ()
    ensureMainValid decls =
      case lookup (StellaIdent "main") decls of
        Nothing -> Left NoMain
        Just (TypeFun [_] _) -> Right ()
        Just (TypeFun args _) -> Left $ WrongArityMain $ length args
        Just _ -> Left NoMain -- main is not a function, but it still abscent, so let it be "NoMain" error

-- Common utils

duplicateIn :: Ord a => [a] -> Maybe a
duplicateIn = go S.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `S.member` seen = Just x
      | otherwise = go (S.insert x seen) xs

-- Typechecking-specific utils

-- Builds a context prefics based on function parameters description
paramsToContext :: [ParamDecl] -> TypeCheckerResult Bindings
paramsToContext decls =
  case duplicateIn [name | (AParamDecl name t) <- decls] of
    Just name -> Left $ DuplicateFunctionParameterName name
    Nothing -> return [(name, t) | (AParamDecl name t) <- decls]

-- Extracts a type of member with label `label` of a type `TypeVariant members`
extractVariantMemberType :: StellaIdent -> [VariantFieldType] -> TypeCheckerResult OptionalTyping
extractVariantMemberType label members = case lookup label [(name, optional_typing) | (AVariantFieldType name optional_typing) <- members] of
  Nothing -> Left $ UnexpectedVariantType label (TypeVariant members)
  Just typing -> return typing

-- Extracts a type of field with name `label` of a type `TypeRecord fields`
extractRecordFieldType :: StellaIdent -> [RecordFieldType] -> TypeCheckerResult Type
extractRecordFieldType label fields = case lookup label [(name, t) | (ARecordFieldType name t) <- fields] of
  Nothing -> Left $ UnexpectedRecordField (TypeRecord fields) label
  (Just t) -> return t

-- Ensures that records value have correct record fiedls
exprSuitsRecordType :: Bool -> [Binding] -> [RecordFieldType] -> TypeCheckerResult ()
exprSuitsRecordType subtypingEnabled actual_bindings expected_bindings
  | not subtypingEnabled && not (all (`elem` expected) actual) = Left $ UnexpectedFields expected actual (Record actual_bindings) (TypeRecord expected_bindings)
  | not (all (`elem` actual) expected) = Left $ MissingFields expected actual (Record actual_bindings) (TypeRecord expected_bindings)
  | otherwise = return ()
  where
    actual = [name | (ABinding name _) <- actual_bindings]
    expected = [name | (ARecordFieldType name _) <- expected_bindings]

-- Ensures that records value have correct record fiedls
patternSuitsRecordType :: [LabelledPattern] -> [RecordFieldType] -> TypeCheckerResult ()
patternSuitsRecordType actual_bindings expected_bindings
  | not (all (`elem` expected) actual) = Left $ UnexpectedPatternForType (PatternRecord actual_bindings) (TypeRecord expected_bindings)
  | not (all (`elem` actual) expected) = Left $ UnexpectedPatternForType (PatternRecord actual_bindings) (TypeRecord expected_bindings)
  | otherwise = return ()
  where
    actual = [name | (ALabelledPattern name _) <- actual_bindings]
    expected = [name | (ARecordFieldType name _) <- expected_bindings]

patternSuitsVariantType :: StellaIdent -> Pattern -> [VariantFieldType] -> TypeCheckerResult OptionalTyping
patternSuitsVariantType label p variants =
  case extractVariantMemberType label variants of
    Left _ -> Left $ UnexpectedPatternForType p $ TypeVariant variants
    Right x -> return x

--Checks well-formed types. Basicly just checks tht records and variants have unique names for fields or members
validateType :: Type -> TypeCheckerResult Type
validateType t@(TypeRecord fields) =
  case duplicateIn [name | (ARecordFieldType name _) <- fields] of
    Nothing -> do
      sequence_ $ validateType <$> [t | (ARecordFieldType _ t) <- fields]
      return t
    Just duplicate -> Left $ DuplicateRecordTypeFields duplicate t
validateType t@(TypeVariant members) =
  case duplicateIn [name | (AVariantFieldType name _) <- members] of
    Nothing -> do
      sequence_ $ validateType <$> [t | (AVariantFieldType _ (SomeTyping t)) <- members]
      return t
    Just duplicate -> Left $ DuplicateVariantTypeMembers duplicate t
validateType t = Right t

-- Joins a contexts produced by several simultaneously matched patterns (e.g. in multivariable let or in PatternTuple)
joinPatternContexts :: [Bindings] -> TypeCheckerResult Bindings
joinPatternContexts contexts =
  let result = concat contexts
   in case duplicateIn [name | (name, _) <- result] of
        Nothing -> return result
        Just duplicated -> Left $ DuplicateVariablePattern duplicated

-- Pattern matching exhaustiveness check utils

-- Checks whether pattern is irrefutable
-- Prerequisite: pattern shall match the type (e.g. checked via patternContext function)
isIrrefutable :: Type -> Pattern -> Bool
isIrrefutable _ (PatternVar _) = True
isIrrefutable TypeUnit PatternUnit = True
isIrrefutable (TypeTuple types) (PatternTuple pats) =
  all (uncurry isIrrefutable) $ zip types pats
isIrrefutable (TypeRecord fields) (PatternRecord pats) =
  all (uncurry isIrrefutable) $ fromRight [] $ sequence [(,pat) <$> extractRecordFieldType name fields | (ALabelledPattern name pat) <- pats]
isIrrefutable _ _ = False

getRawIdent :: StellaIdent -> String
getRawIdent (StellaIdent a) = a

namesInPattern :: Pattern -> [StellaIdent]
namesInPattern (PatternAsc pat _) = namesInPattern pat
namesInPattern (PatternVariant _ (SomePatternData pat)) = namesInPattern pat
namesInPattern (PatternVariant _ NoPatternData) = []
namesInPattern (PatternInl pat) = namesInPattern pat
namesInPattern (PatternInr pat) = namesInPattern pat
namesInPattern (PatternTuple pats) = concatMap namesInPattern pats
namesInPattern (PatternRecord pats) = concatMap namesInPattern ([pat | (ALabelledPattern _ pat) <- pats])
namesInPattern (PatternList pats) = concatMap namesInPattern pats
namesInPattern (PatternCons a b) = namesInPattern a ++ namesInPattern b
namesInPattern PatternFalse = []
namesInPattern PatternTrue = []
namesInPattern PatternUnit = []
namesInPattern (PatternInt _) = []
namesInPattern (PatternSucc pat) = namesInPattern pat
namesInPattern (PatternVar name) = [name]
namesInPattern (PatternCastAs pat _) = namesInPattern pat

-- Generalised representation of albebraical data type variant
data Constructor a = Constructor
  { constrOver :: Type,
    constrName :: String,
    constrChildren :: [a]
  }
  deriving (Eq, Show, Read)

constrArity :: Constructor a -> Int
constrArity = length . constrChildren

-- Checks whether pattern constructor matches againts specific constructor
constructorMatchesImpl :: Constructor a -> Constructor b -> Bool
constructorMatchesImpl consPat consType = constrOver consPat == constrOver consType && constrName consPat == constrName consType

patternMatchesConstructor :: Pattern -> Constructor Type -> Bool
patternMatchesConstructor p c =
  case matchedConstructor (constrOver c) p of
    Nothing -> False
    Just p -> constructorMatchesImpl p c

-- Lists the constructors for type
listConstructors :: Type -> [Constructor Type]
listConstructors TypeUnit = [Constructor TypeUnit "unit" []]
listConstructors TypeBool = [Constructor TypeBool "true" [], Constructor TypeBool "false" []]
listConstructors TypeNat = [Constructor TypeNat "zero" [], Constructor TypeNat "succ" [TypeNat]]
listConstructors t@(TypeList elem) = [Constructor t "nil" [], Constructor t "cons" [elem, t]]
listConstructors t@(TypeSum l r) = [Constructor t "inl" [l], Constructor t "inr" [r]]
listConstructors t@(TypeVariant members) = [Constructor t (getRawIdent name) (optionalTypingToArr ty) | (AVariantFieldType name ty) <- members]
  where
    optionalTypingToArr (SomeTyping t) = [t]
    optionalTypingToArr NoTyping = []
listConstructors t@(TypeTuple fields) = [Constructor t "tuple" fields]
listConstructors t@(TypeRecord fields) = [Constructor t "tuple" [t | (ARecordFieldType _ t) <- fields]]
listConstructors t@(TypeTop) = [] -- This is wrong
listConstructors t = error $ "Exhaustiveness check internal error " ++ printTree t -- Other types are non-matchable. Should be checked via patternContext prior to exhaustiveness check

-- Evaluates which constructor of known type is covered with known pattern. Nothing stands for all the constructors
matchedConstructor :: Type -> Pattern -> Maybe (Constructor Pattern)
matchedConstructor t (PatternVar _) = Nothing
matchedConstructor t (PatternAsc p _) = matchedConstructor t p
matchedConstructor t (PatternCastAs p t') = matchedConstructor t' p
matchedConstructor TypeBool PatternTrue = Just $ Constructor TypeBool "true" []
matchedConstructor TypeBool PatternFalse = Just $ Constructor TypeBool "false" []
matchedConstructor TypeUnit PatternUnit = Just $ Constructor TypeUnit "unit" []
matchedConstructor t p@(PatternInt 0) = Just $ Constructor t "zero" []
matchedConstructor t p@(PatternInt n) = error $ "Exhaustiveness check internal error:\n  " ++ printTree p ++ "\n  " ++ printTree t
matchedConstructor t p@(PatternSucc ch) = Just $ Constructor t "succ" [ch]
matchedConstructor t (PatternInl pattern) = Just $ Constructor t "inl" [pattern]
matchedConstructor t (PatternInr pattern) = Just $ Constructor t "inr" [pattern]
matchedConstructor t p@(PatternList []) = Just $ Constructor t "nil" []
matchedConstructor t p@(PatternList (h : t')) = error $ "Exhaustiveness check internal error:\n  " ++ printTree p ++ "\n  " ++ printTree t
matchedConstructor t@(TypeList elem) (PatternCons h t') = Just $ Constructor t "cons" [h, t']
matchedConstructor t@(TypeRecord fields) (PatternRecord pats) =
  Just $
    Constructor t "record" $
      fromJust $
        sequence [lookup label patsMap | (ARecordFieldType label t) <- fields]
  where
    patsMap = [(label, pat) | (ALabelledPattern label pat) <- pats]
matchedConstructor t (PatternTuple fields) = Just $ Constructor t "tuple" fields
matchedConstructor t (PatternVariant label (SomePatternData inner)) = Just $ Constructor t (getRawIdent label) [inner]
matchedConstructor t (PatternVariant label NoPatternData) = Just $ Constructor t (getRawIdent label) []
matchedConstructor t p = error $ "Exhaustiveness check internal error:\n  " ++ printTree p ++ "\n  " ++ printTree t

-- Removes all the (PatternInt n) where n > 0 and (PatternList elems) where elems is not empty
-- TODO: rewrite Pattern Type with `Fix` to simplify recursive application of desugarPattern
desugarPattern :: Pattern -> Pattern
desugarPattern (PatternAsc p t) = PatternAsc (desugarPattern p) t
desugarPattern (PatternCastAs p t) = PatternCastAs (desugarPattern p) t
desugarPattern p@(PatternInt 0) = p
desugarPattern (PatternInt n) = PatternSucc $ desugarPattern (PatternInt $ n - 1)
desugarPattern (PatternSucc ch) = PatternSucc $ desugarPattern ch
desugarPattern (PatternInl pattern) = PatternInl $ desugarPattern pattern
desugarPattern (PatternInr pattern) = PatternInr $ desugarPattern pattern
desugarPattern p@(PatternList []) = p
desugarPattern p@(PatternList (h : t)) = PatternCons h $ desugarPattern $ PatternList t
desugarPattern (PatternCons h t) = PatternCons (desugarPattern h) $ desugarPattern t
desugarPattern (PatternRecord fields) = PatternRecord [ALabelledPattern name $ desugarPattern pat | (ALabelledPattern name pat) <- fields]
desugarPattern (PatternTuple fields) = PatternTuple $ desugarPattern <$> fields
desugarPattern (PatternVariant label (SomePatternData inner)) = PatternVariant label $ SomePatternData $ desugarPattern inner
desugarPattern p = p

-- Given a matrix of patterns returns whether sum of all rows of matrix covers all the cases
-- of given types. One column of matrix corresponds one type in a list of types
allCovered :: [[Pattern]] -> [Type] -> Bool
allCovered rows [] = not . null $ rows
allCovered rows (t : rest) =
  let firstColumn = map head rows
   in if all (isIrrefutable t) firstColumn
        then allCovered (map tail rows) rest
        else
          let constructorsToCover = listConstructors t
           in all (matchesConstructor rows rest) constructorsToCover
  where
    headMatches :: Constructor Type -> [Pattern] -> Bool
    headMatches constr (p : _) =
      isIrrefutable (constrOver constr) p
        || patternMatchesConstructor p constr
    headMatches _ [] = False
    matchesConstructor :: [[Pattern]] -> [Type] -> Constructor Type -> Bool
    matchesConstructor rows ts c =
      let matchedRows = filter (headMatches c) rows
       in not (null matchedRows)
            && ( let extendedRows = concatMap (extendRow c) matchedRows
                  in allCovered extendedRows (constrChildren c ++ ts)
               )
    extendRow :: Constructor Type -> [Pattern] -> [[Pattern]]
    extendRow c r@(h : rest) =
      if isIrrefutable (constrOver c) h
        then [replicate (constrArity c) (PatternVar (StellaIdent "_")) ++ rest]
        else case matchedConstructor (constrOver c) h of
          Nothing -> []
          Just pats -> [constrChildren pats ++ rest]
    extendRow c [] = []

-- Finds first non-expected type and returns an erorr
findNonExpected :: Context -> Type -> [(Type, Expr)] -> TypeCheckerResult ()
findNonExpected ctx expected ((t, e) : rest) = do
  contextCheckIsSuitable ctx e t expected
  findNonExpected ctx expected rest
findNonExpected _ _ [] = return ()

-- Checks whether structural pattern binding is exhaustive
-- Prerequidite: patternContext does not emit error for any of patterns
isExhaustiveStructural :: Type -> [Pattern] -> Bool
isExhaustiveStructural t patterns = let rows = [[desugarPattern pat] | pat <- patterns] in allCovered rows [t]

-- Produces the context which consists with binded names after matching expression of type `t`
-- against specific pattern
patternContext :: Pattern -> Type -> TypeCheckerResult Bindings
patternContext (PatternVar name) t = return [(name, t)]
patternContext (PatternCastAs p ty) t = do
  unless (ty <: t) (Left $ UnexpectedSubtypeNoExpr t ty)
  patternContext p ty
patternContext (PatternAsc p t') t = do
  -- unless (t' == t) (Left $ UnexpectedPatternForType p t')
  patternContext p t
patternContext (PatternInl pattern) (TypeSum l _) = patternContext pattern l
patternContext PatternUnit TypeUnit = Right []
patternContext PatternFalse TypeBool = Right []
patternContext PatternTrue TypeBool = Right []
patternContext (PatternInt _) TypeNat = Right []
patternContext (PatternSucc p) TypeNat = patternContext p TypeNat
patternContext p@(PatternInl pattern) t = Left $ UnexpectedPatternForType p t
patternContext p@(PatternList patterns) (TypeList elemType) =
  sequence [patternContext pat elemType | pat <- patterns] >>= joinPatternContexts
patternContext p@(PatternCons headPattern tailPattern) t@(TypeList elemType) = do
  a <- patternContext headPattern elemType
  b <- patternContext tailPattern t
  joinPatternContexts [a, b]
patternContext p@(PatternRecord labels) (TypeRecord fields) = do
  case duplicateIn [name | (ALabelledPattern name pat) <- labels] of
    Nothing -> return ()
    Just duplicated -> Left $ DuplicateRecordPattern duplicated p
  patternSuitsRecordType labels fields
  let fields_patterns = [(name, pat) | (ALabelledPattern name pat) <- labels]
  ctxs <- sequence [extractRecordFieldType name fields >>= patternContext pat | (ALabelledPattern name pat) <- labels]
  joinPatternContexts ctxs
patternContext p@(PatternTuple pats) t@(TypeTuple fields) = do
  unless (length pats == length fields) (Left $ UnexpectedPatternForType p t)
  sequence [patternContext pat ty | (pat, ty) <- zip pats fields] >>= joinPatternContexts
patternContext (PatternInr pattern) (TypeSum _ r) = patternContext pattern r
patternContext p@(PatternInr pattern) t = Left $ UnexpectedPatternForType p t
patternContext p@(PatternVariant label (SomePatternData inner)) t@(TypeVariant members) = do
  typing <- patternSuitsVariantType label p members
  case typing of
    NoTyping -> Left $ UnexpectedNonNullaryPattern label p t
    SomeTyping ty -> patternContext inner ty
patternContext p@(PatternVariant label NoPatternData) t@(TypeVariant members) = do
  typing <- patternSuitsVariantType label p members
  case typing of
    NoTyping -> return []
    SomeTyping ty -> Left $ UnexpectedNullaryPattern label p t
patternContext p t = Left $ UnexpectedPatternForType p t

-- Just infer + patternContext for PatternBinding
patternBindingContext :: Context -> PatternBinding -> TypeCheckerResult Bindings
patternBindingContext ctx (APatternBinding pattern expr) = do
  t <- infer ctx expr
  res <- patternContext pattern t
  unless (isIrrefutable t pattern) (Left $ NonexhaustivePatternMatching [pattern] expr t)
  return res

ensureNoRecordDuplicateFileds :: [Binding] -> TypeCheckerResult ()
ensureNoRecordDuplicateFileds bindings =
  case duplicateIn [name | (ABinding name e) <- bindings] of
    Just ident -> Left $ DuplicateRecordFields ident $ Record bindings
    Nothing -> return ()

-- Subtyping utils
(<:) :: Type -> Type -> Bool
(TypeFun lhs'args lhs'ret) <: (TypeFun rhs'args rhs'ret) =
  length lhs'args == length rhs'args
    && all (uncurry (<:)) (zip rhs'args lhs'args)
    && lhs'ret <: rhs'ret
(TypeRecord lhs'fields) <: (TypeRecord rhs'fields) =
  all (`elem` lhs'names) rhs'names
    && all (uncurry (<:) . getBothTypes) rhs'names
  where
    lhs'names = [name | (ARecordFieldType name _) <- lhs'fields]
    rhs'names = [name | (ARecordFieldType name _) <- rhs'fields]
    getBothTypes :: StellaIdent -> (Type, Type)
    getBothTypes name = case (,) <$> extractRecordFieldType name lhs'fields <*> extractRecordFieldType name rhs'fields of
      Left _ -> error "Internal typechecker error"
      Right ts -> ts
(TypeTuple lhs) <: (TypeTuple rhs) =
  length lhs == length rhs
    && all (uncurry (<:)) (zip lhs rhs)
(TypeSum ll lr) <: (TypeSum rl rr) =
  ll <: rl && lr <: rr
(TypeList lhs) <: (TypeList rhs) =
  lhs <: rhs
(TypeRef lhs) <: (TypeRef rhs) = lhs <: rhs && rhs <: lhs
(TypeVariant lhs'fields) <: (TypeVariant rhs'fields) =
  all (`elem` rhs'names) lhs'names
    && all (uncurry (<::) . getBothTypes) lhs'names
  where
    lhs'names = [name | (AVariantFieldType name _) <- lhs'fields]
    rhs'names = [name | (AVariantFieldType name _) <- rhs'fields]
    getBothTypes :: StellaIdent -> (OptionalTyping, OptionalTyping)
    getBothTypes name = case (,) <$> extractVariantMemberType name lhs'fields <*> extractVariantMemberType name rhs'fields of
      Left _ -> error "Internal typechecker error"
      Right ts -> ts
    (<::) :: OptionalTyping -> OptionalTyping -> Bool
    NoTyping <:: NoTyping = True
    SomeTyping l <:: SomeTyping r = l <: r
    _ <:: _ = False
TypeBottom <: _ = True
_ <: TypeTop = True
l <: r = l == r

-- inference function: calculates type of expression based on its structure and context
-- Context contains information of externally defined variables types, with respect to possible shadowing
-- TODO: Monad reader for configs ???
infer :: Context -> Expr -> TypeCheckerResult Type
infer _ ConstTrue = return TypeBool
infer _ ConstFalse = return TypeBool
infer ctx (If c t e) = do
  ensure ctx c TypeBool
  lhs <- infer ctx t
  ensure ctx e lhs
  return lhs
infer ctx (Abstraction params body) = do
  _ <- paramsToContext params
  sequence_ $ validateType <$> [t | (AParamDecl _ t) <- params]
  return_type <- infer (contextExtend ctx [(name, t) | (AParamDecl name t) <- params]) body
  return $ TypeFun [t | (AParamDecl name t) <- params] return_type
infer ctx e@(Application callee args) = do
  callee_type <- infer ctx callee
  case callee_type of
    (TypeFun params return_type) -> do
      unless (length params == length args) (Left $ MismatchedArgumentsNumber (length params) (length args) e)
      sequence_ [ensure ctx a p | (p, a) <- zip params args]
      return return_type
    _ -> Left $ NotAFunction callee
infer _ (ConstInt _) = return TypeNat
infer ctx (Succ num) = do
  ensure ctx num TypeNat
  return TypeNat
infer ctx (IsZero num) = do
  ensure ctx num TypeNat
  return TypeBool
infer ctx (NatRec num init step) = do
  ensure ctx num TypeNat
  t <- infer ctx init
  ensure ctx step (TypeFun [TypeNat] $ TypeFun [t] t)
  return t
infer ctx (Var name) =
  contextLookup ctx name
infer _ ConstUnit = return TypeUnit
infer ctx (Tuple elems) = TypeTuple <$> sequence (infer ctx <$> elems)
infer ctx (DotTuple tuple index) = do
  tuple_type <- infer ctx tuple
  case tuple_type of
    (TypeTuple elems_types) ->
      if fromEnum index > 0 && fromEnum index <= length elems_types
        then return $ elems_types !! (fromEnum index - 1)
        else Left $ TupleIndexOutOfBounds index tuple_type
    _ -> Left $ NotATuple tuple
infer ctx (Record bindings) = do
  ensureNoRecordDuplicateFileds bindings
  element_types <- sequence $ [(name,) <$> infer ctx e | (ABinding name e) <- bindings]
  validateType $ TypeRecord $ uncurry ARecordFieldType <$> element_types
infer ctx (DotRecord record field) = do
  record_type <- infer ctx record
  case record_type of
    (TypeRecord fields_types) ->
      case lookup field [(name, t) | (ARecordFieldType name t) <- fields_types] of
        Just t -> return t
        Nothing -> Left $ UnexpectedRecordField record_type field
    _ -> Left $ NotARecord record
infer ctx (Let bindings body) = do
  let bindedNames = concatMap namesInPattern [pat | (APatternBinding pat _) <- bindings]
  case duplicateIn bindedNames of
    Just name -> Left $ DuplicateVariableLet name
    Nothing -> do
      patternCtxs <- sequence $ patternBindingContext ctx <$> bindings
      patternsCtx <- joinPatternContexts patternCtxs
      infer (contextExtend ctx patternsCtx) body
infer ctx (LetRec [APatternBinding (PatternAsc pattern t) expr] body) = do
  let bindedNames = namesInPattern pattern
  case duplicateIn bindedNames of
    Just name -> Left $ DuplicateVariableLet name
    Nothing -> do
      ctx' <- patternContext pattern t
      unless (isIrrefutable t pattern) (Left $ NonexhaustivePatternMatching [pattern] expr t)
      let extendedCtx = contextExtend ctx ctx'
      ensure extendedCtx expr t
      infer extendedCtx body
infer ctx (LetRec [APatternBinding pattern expr] body) = Left $ AmbigousPatternType pattern
infer ctx (LetRec _ body) = Left $ UnsupportedConstruction "letrec with many bindings"
infer ctx (TypeAsc e t) =
  do
    validateType t
    ensure ctx e t
    return t
infer ctx (Inl e) =
  if ctxAmbigiousisBottom ctx
    then do
      t <- infer ctx e
      return $ TypeSum t TypeBottom
    else Left AmbigousSumType
infer ctx (Inr e) =
  if ctxAmbigiousisBottom ctx
    then do
      t <- infer ctx e
      return $ TypeSum TypeBottom t
    else Left AmbigousSumType
infer ctx (Match e []) = Left EmptyMatch
infer ctx (Match e cases@(c : cs)) = do
  t <- infer ctx e
  let patterns = [pattern | (AMatchCase pattern _) <- cases]
  branches_types <- sequence [patternContext pattern t >>= (\ctx' -> infer (contextExtend ctx ctx') b) | (AMatchCase pattern b) <- cases]
  let expected_type = head branches_types
  let rest_types = tail branches_types
  let rest_exprs = [b | (AMatchCase pattern b) <- cs]
  findNonExpected ctx expected_type $ zip rest_types rest_exprs
  if not (isExhaustiveStructural t patterns)
    then Left $ NonexhaustivePatternMatching patterns e t
    else return expected_type
infer ctx (List []) =
  if ctxAmbigiousisBottom ctx
    then return $ TypeList TypeBottom
    else Left AmbigousListType
infer ctx (List exprs@(h : t)) = do
  head_type <- infer ctx h
  tails_types <- sequence $ infer ctx <$> t
  findNonExpected ctx head_type $ zip tails_types t
  return $ TypeList head_type
infer ctx (ConsList head tail) = do
  t <- infer ctx head
  ensure ctx tail (TypeList t)
  return (TypeList t)
infer ctx (Head list) = do
  t <- infer ctx list
  case t of
    (TypeList e) -> return e
    _ -> Left $ NotAList list
infer ctx (Tail list) = do
  t <- infer ctx list
  case t of
    (TypeList e) -> return t
    _ -> Left $ NotAList list
infer ctx (IsEmpty list) = do
  t <- infer ctx list
  case t of
    (TypeList e) -> return TypeBool
    _ -> Left $ NotAList list
infer ctx (Variant label (SomeExprData expr)) =
  if ctxSubtyping ctx
    then do
      t <- infer ctx expr
      return $ TypeVariant [AVariantFieldType label (SomeTyping t)]
    else Left AmbigousVariantType
infer ctx (Variant label NoExprData) =
  if ctxSubtyping ctx
    then do
      return $ TypeVariant [AVariantFieldType label NoTyping]
    else Left AmbigousVariantType
infer ctx e@(Fix f) = do
  arrow <- infer ctx f
  case arrow of
    (TypeFun [arg] ret) ->
      if arg == ret
        then return arg
        else Left $ UnexpectedType f (TypeFun [ret] ret) arrow
    (TypeFun args _) -> Left $ MismatchedArgumentsNumber 1 (length args) e
    _ -> Left $ NotAFunction f
infer ctx (Sequence former latter) = do
  ensure ctx former TypeUnit
  infer ctx latter
infer ctx (Ref expr) = do
  t <- infer ctx expr
  return $ TypeRef t
infer ctx (Deref expr) = do
  ref_t <- infer ctx expr
  case ref_t of
    TypeRef t -> return t
    t' -> Left $ NotAReference expr
infer ctx (Assign ref expr) = do
  ref_t <- infer ctx ref
  case ref_t of
    TypeRef t -> do
      ensure ctx expr t
      return TypeUnit
    t' -> Left $ NotAReference ref
infer ctx e@(ConstMemory _) = Left $ AmbigousReferenceType e -- TODO: add memory typing ???
infer ctx e@Panic =
  if ctxAmbigiousisBottom ctx
    then return TypeBottom
    else Left $ AmbigousPanicType e
infer ctx e@(Throw exception) = do
  exceptionType <- contextCurrentException ctx
  ensure ctx exception exceptionType
  if ctxAmbigiousisBottom ctx
    then return TypeBottom
    else Left $ AmbigousThrowType e
infer ctx (TryCatch s p e) = do
  t <- infer ctx s
  exc <- contextCurrentException ctx
  bindings <- patternContext p exc
  let ctx' = contextExtend ctx bindings
  ensure ctx' e t
  return t
infer ctx (TryWith s e) = do
  t <- infer ctx s
  ensure ctx e t
  return t
infer ctx e@(TryCastAs expr ty pattern success fail) = do
  t <- infer ctx expr
  unless (ty <: t) (Left $ UnexpectedSubtypeNoExpr t ty)
  ctx' <- patternContext pattern ty
  unless (isIrrefutable ty pattern) (Left $ NonexhaustivePatternMatching [pattern] expr ty)
  t' <- infer (contextExtend ctx ctx') success
  ensure ctx fail t'
  return t'
infer _ e = Left $ UnsupportedExpression e

-- verification function: ensures that expression can be typed with the specific type
-- Context contains information of externally defined variables types, with respect to possible shadowing
-- Default case is type inference and comparasion
ensure :: Context -> Expr -> Type -> TypeCheckerResult ()
ensure _ ConstTrue TypeBool = return ()
ensure _ ConstFalse TypeBool = return ()
ensure ctx (If c t e) expected = do
  ensure ctx c TypeBool
  ensure ctx t expected
  ensure ctx e expected
ensure ctx e@(Abstraction params body) (TypeFun expected_args return_type) = do
  _ <- paramsToContext params
  let actual_args = [t | (AParamDecl name t) <- params]
  if length expected_args /= length actual_args
    then Left $ UnexpectedArgumentsNumberInLambda (length expected_args) (length actual_args) e
    else
      if all (uncurry $ flip $ contextIsSuitable ctx) $ zip actual_args expected_args
        then ensure (contextExtend ctx [(name, t) | (AParamDecl name t) <- params]) body return_type
        else
          Left $
            UnexpectedTypeForParameter
              expected_args
              actual_args
ensure ctx e@(Abstraction _ _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedLambda e t)
  | otherwise = Left $ UnexpectedLambda e t
ensure _ (ConstInt _) TypeNat = return ()
ensure ctx (NatRec num init step) expected = do
  ensure ctx num TypeNat
  ensure ctx init expected
  ensure ctx step (TypeFun [TypeNat] $ TypeFun [expected] expected)
ensure _ ConstUnit TypeUnit = return ()
ensure ctx t@(Tuple elems) e@(TypeTuple tuple_elems)
  | length elems /= length tuple_elems = Left $ UnexpectedTupleLength (length tuple_elems) (length elems) e t
  | otherwise = sequence_ [ensure ctx e t | (e, t) <- zip elems tuple_elems]
ensure ctx e@(Tuple _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedTuple e t)
  | otherwise = Left $ UnexpectedTuple e t
ensure ctx (Record bindings) t@(TypeRecord fields) = do
  _ <- validateType t
  ensureNoRecordDuplicateFileds bindings
  exprSuitsRecordType (ctxSubtyping ctx) bindings fields
  sequence_ [ensureFieldType name e fields | (ABinding name e) <- bindings]
  where
    ensureFieldType name e fiedls = case extractRecordFieldType name fields of
      Left _ -> void (infer ctx e)
      Right ty -> ensure ctx e ty
ensure ctx e@(Record _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedRecord e t)
  | otherwise = Left $ UnexpectedRecord e t
ensure ctx (Inl e) (TypeSum l _) = ensure ctx e l
ensure ctx e@(Inl _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedInjection e t)
  | otherwise = Left $ UnexpectedInjection e t
ensure ctx (Inr e) (TypeSum _ r) = ensure ctx e r
ensure ctx e@(Inr _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedInjection e t)
  | otherwise = Left $ UnexpectedInjection e t
ensure ctx (Match e cases@(c : cs)) expected = do
  t <- infer ctx e
  let patterns = [pattern | (AMatchCase pattern _) <- cases]
  sequence_ [patternContext pattern t >>= (\ctx' -> ensure (contextExtend ctx ctx') b expected) | (AMatchCase pattern b) <- cases]
  unless (isExhaustiveStructural t patterns) $ Left $ NonexhaustivePatternMatching patterns e t
ensure ctx (List []) (TypeList _) = return ()
ensure ctx (List exprs) (TypeList expected) = sequence_ $ [ensure ctx expr expected | expr <- exprs]
ensure ctx e@(List _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedList e t)
  | otherwise = Left $ UnexpectedList e t
ensure ctx (ConsList head tail) t@(TypeList expected) = do
  ensure ctx head expected
  ensure ctx tail t
ensure ctx e@(ConsList _ _) t
  | ctxSubtyping ctx = do
    t' <- infer ctx e
    unless (t' <: t) (Left $ UnexpectedList e t)
  | otherwise = Left $ UnexpectedList e t
ensure ctx (Head list) expected = ensure ctx list (TypeList expected)
ensure ctx (Tail list) expected@(TypeList _) = ensure ctx list expected
ensure ctx e@(Variant label (SomeExprData expr)) t@(TypeVariant members) = do
  expected <- extractVariantMemberType label members
  case expected of
    NoTyping -> Left $ UnexpectedDataForNullaryType label e t
    SomeTyping ty -> ensure ctx expr ty
ensure ctx (Let bindings body) t = do
  let bindedNames = concatMap namesInPattern [pat | (APatternBinding pat _) <- bindings]
  case duplicateIn bindedNames of
    Just name -> Left $ DuplicateVariableLet name
    Nothing -> do
      patternCtxs <- sequence $ patternBindingContext ctx <$> bindings
      patternsCtx <- joinPatternContexts patternCtxs
      ensure (contextExtend ctx patternsCtx) body t
ensure ctx (LetRec [APatternBinding (PatternAsc pattern t) expr] body) t' = do
  ctx' <- patternContext pattern t
  unless (isIrrefutable t pattern) (Left $ NonexhaustivePatternMatching [pattern] expr t)
  let extendedCtx = contextExtend ctx ctx'
  ensure extendedCtx expr t
  ensure extendedCtx body t'
ensure ctx (LetRec [APatternBinding pattern expr] body) _ = Left $ AmbigousPatternType pattern
ensure ctx (LetRec _ body) _ = Left $ UnsupportedConstruction "letrec with many bindings"
ensure ctx e@(Variant label NoExprData) t@(TypeVariant members) = do
  expected <- extractVariantMemberType label members
  case expected of
    NoTyping -> return ()
    SomeTyping _ -> Left $ MissingDataForMember label e t
ensure ctx e@(Variant label (SomeExprData expr)) t = Left $ UnexpectedVariant e t
ensure ctx (Fix f) expected = case ensure ctx f (TypeFun [expected] expected) of
  Left err -> case infer ctx f of
    Left _ -> Left err
    Right i@(TypeFun _ _) -> Left $ UnexpectedType f (TypeFun [expected] expected) i
    Right e -> Left $ NotAFunction f
  res -> res
ensure ctx (Sequence former latter) t = do
  ensure ctx former TypeUnit
  ensure ctx latter t
ensure ctx (Ref expr) (TypeRef t) = do
  ensure ctx expr t
ensure ctx e@(Ref expr) t = Left $ UnexpectedReference e t
ensure ctx e@(Deref expr) t =
  if ctxSubtyping ctx
    then case ensure ctx expr (TypeRef t) of
      Right () -> Right ()
      Left _ -> do
        infered <- infer ctx expr
        case infered of
          TypeRef t' -> contextCheckIsSuitable ctx expr t' t
          _ -> Left $ NotAReference expr
    else ensure ctx expr (TypeRef t)
ensure ctx e@(ConstMemory _) (TypeRef _) = return () -- TODO: where can I find
ensure ctx e@(ConstMemory _) t = Left $ UnexpectedMemoryAddress e t
ensure ctx Panic t = return ()
ensure ctx e@(Throw exception) _ = do
  exceptionType <- contextCurrentException ctx
  ensure ctx exception exceptionType
ensure ctx (TryCatch s p e) t = do
  ensure ctx s t
  exc <- contextCurrentException ctx
  bindings <- patternContext p exc
  let ctx' = contextExtend ctx bindings
  ensure ctx' e t
ensure ctx (TryWith s e) t = do
  ensure ctx s t
  ensure ctx e t
ensure ctx e@(TryCastAs expr ty pattern success fail) t = do
  t' <- infer ctx expr
  unless (ty <: t') (Left $ UnexpectedSubtypeNoExpr t' ty)
  ctx' <- patternContext pattern ty
  unless (isIrrefutable ty pattern) (Left $ NonexhaustivePatternMatching [pattern] expr ty)
  ensure (contextExtend ctx ctx') success t
  ensure ctx fail t
ensure ctx expr expected = do
  infered <- infer ctx expr
  contextCheckIsSuitable ctx expr infered expected

-- validate target + ensure
validate'n'ensure :: Context -> Expr -> Type -> TypeCheckerResult ()
validate'n'ensure ctx e t = do
  t' <- validateType t
  ensure ctx e t'

-- validate context + validate target + ensure
validate'n'ensure'ctx :: Context -> Expr -> Type -> TypeCheckerResult ()
validate'n'ensure'ctx ctx e t = do
  sequence_ $ validateType . snd <$> ctxBindings ctx
  validate'n'ensure ctx e t

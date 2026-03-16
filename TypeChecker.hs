{-# LANGUAGE TupleSections #-}

-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Syntax.TypeChecker where

import qualified Control.Monad
import Data.Foldable (sequenceA_)
import Data.List (intercalate, nub, (\\))
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import Debug.Trace (trace)
import Syntax.Abs
  ( Binding (ABinding),
    Decl (DeclFun),
    Expr
      ( Abstraction,
        Application,
        ConsList,
        ConstFalse,
        ConstInt,
        ConstTrue,
        ConstUnit,
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
        List,
        Match,
        NatRec,
        Record,
        Succ,
        Tail,
        Tuple,
        TypeAsc,
        Var,
        Variant
      ),
    ExprData (NoExprData, SomeExprData),
    MatchCase (AMatchCase),
    OptionalTyping (NoTyping, SomeTyping),
    ParamDecl (AParamDecl),
    Pattern (PatternInl, PatternInr, PatternRecord, PatternTuple, PatternVar, PatternVariant),
    PatternBinding (..),
    PatternData (NoPatternData, SomePatternData),
    Program (..),
    RecordFieldType (ARecordFieldType),
    ReturnType (SomeReturnType),
    StellaIdent (StellaIdent),
    Type
      ( TypeBool,
        TypeFun,
        TypeList,
        TypeNat,
        TypeRecord,
        TypeSum,
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
  | NotAFunction Expr
  | NotATuple Expr
  | NotARecord Expr
  | NotAList Expr
  | UnexpectedTupleLength Int Int Type Expr
  | DuplicateVariablePattern
  | DuplicateRecordFields StellaIdent Type
  | DuplicateVariantType StellaIdent Type
  | DuplicateFunctionDeclaration StellaIdent
  | AmbigousSumType
  | AmbigousVariantType
  | AmbigousListType
  | UnexpectedRecordField Type StellaIdent
  | UnexpectedTypeForParameter [Type] [Type]
  | UnsupportedExpression Expr
  | UnsupportedDecl Decl
  | UnsupportedPattern Pattern
  | UnsupportedConstruction String
  | MismatchedBranchesTypes
  | MismatchedListTypes
  | MissingFields [StellaIdent] [StellaIdent] Expr Type
  | UnexpectedFields [StellaIdent] [StellaIdent] Expr Type
  | EmptyMatch
  | UnexpectedLambda Expr Type
  | UnexpectedTuple Expr Type
  | UnexpectedRecord Expr Type
  | UnexpectedInjection Expr Type
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
  | NonexhaustivePatternMatching [Pattern] Expr Type
  deriving (Eq, Ord, Read)

instance Show TypeCheckerError where
  show err = case err of
    NoMain ->
      "ERROR_MISSING_MAIN:\n  program has no main function"
    WrongArityMain n ->
      "ERROR_INCORRECT_ARITY_OF_MAIN\n  main have " ++ show n ++ " arguments"
    DuplicateFunctionDeclaration name -> "ERROR_DUPLICATE_FUNCTION_DECLARATION:\n  Function " ++ show name ++ " declaraded several times"
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
    NotAFunction expr ->
      "ERROR_NOT_A_FUNCTION:\n  expression is not a function: " ++ printTree expr
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
    DuplicateVariablePattern ->
      "ERROR_DUPLICATE_VARIABLE_PATTERN:\n  duplicate variable in pattern"
    DuplicateRecordFields field t ->
      "ERROR_DUPLICATE_RECORD_FIELDS:\n  duplicate field: " ++ show field ++ " in record type " ++ printTree t
    DuplicateVariantType field t ->
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
    MismatchedBranchesTypes ->
      "ERROR_MISMATCHED_BRANCHES_TYPES:\n  branches in match have different types"
    MismatchedListTypes ->
      "ERROR_MISMATCHED_LIST_TYPES:\n  list elements have inconsistent types"
    EmptyMatch ->
      "ERROR_ILLEGAL_EMPTY_MATCHING:\n  match expression with no branches"
    UnexpectedPatternForType pat ty ->
      "ERROR_UNEXPECTED_PATTERN_FOR_TYPE:\n"
        ++ "  pattern "
        ++ printTree pat
        ++ " does not match type "
        ++ printTree ty
    MismatchedArgumentsNumber expected actual expr ->
      "ERROR_MISMATCHED_ARGUMENTS_NUMBER:\n"
        ++ "  expected "
        ++ show expected
        ++ " arguments but got "
        ++ show actual
        ++ " in application:\n    "
        ++ printTree expr
    UnexpectedArgumentsNumberInLambda expected actual expr ->
      "ERROR_UNEXPECTED_ARGUMENTS_NUMBER_IN_LAMBDA:\n"
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

type TypeCheckerResult t = Either TypeCheckerError t

type Context = [(StellaIdent, Type)]

-- Entry point of type checker: checks the whole program
typeCheck :: Program -> TypeCheckerResult ()
typeCheck program@(AProgram _ _ decls) = do
  globalDecls <- collectFuncDecls program
  sequence_ $ typeCheckDecl globalDecls <$> decls
  where
    typeCheckDecl globalDecls (DeclFun _ name params (SomeReturnType return_type) _ _ body) =
      validate'n'ensure'ctx (paramsToContext params ++ globalDecls) body return_type
    typeCheckDecl _ _ = return ()
    extractFunctionType :: Decl -> TypeCheckerResult (StellaIdent, Type)
    extractFunctionType (DeclFun _ name params (SomeReturnType return_type) _ _ _) = Right (name, TypeFun [t | (AParamDecl _ t) <- params] return_type)
    extractFunctionType decl = Left $ UnsupportedDecl decl
    collectFuncDecls :: Program -> TypeCheckerResult Context
    collectFuncDecls (AProgram _ _ decls) = ensureDeclsCorrectness . sequence $ extractFunctionType <$> decls
    ensureDeclsCorrectness :: TypeCheckerResult Context -> TypeCheckerResult Context
    ensureDeclsCorrectness decls_result = do
      decls <- decls_result
      case lookup (StellaIdent "main") decls of
        Nothing -> Left NoMain
        Just (TypeFun [_] _) -> case duplicateIn [name | (name, _) <- decls] of
          Nothing -> return decls
          (Just duplicated) -> Left $ DuplicateFunctionDeclaration duplicated
        Just (TypeFun args _) -> Left $ WrongArityMain $ length args
        Just _ -> Left NoMain

-- Common utils

duplicateIn :: Ord a => [a] -> Maybe a
duplicateIn = go S.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `S.member` seen = Just x
      | otherwise = go (S.insert x seen) xs

haveNoDuplicates :: Eq a => [a] -> Bool
haveNoDuplicates l = length l == length (nub l)

ensureNoDuplicates :: Eq a => [a] -> e -> Either e ()
ensureNoDuplicates l err = if haveNoDuplicates l then Right () else Left err

areEqualAsSets :: Ord a => [a] -> [a] -> Bool
areEqualAsSets xs ys = S.fromList xs == S.fromList ys

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

-- Typechecking-specific utils

-- Builds a context prefics based on function parameters description
paramsToContext :: [ParamDecl] -> Context
paramsToContext decls = [(name, t) | (AParamDecl name t) <- decls]

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

-- Ensures that two records types are the same type
suitsRecordType :: [Binding] -> [RecordFieldType] -> TypeCheckerResult ()
suitsRecordType actual_bindings expected_bindings
  | not (all (`elem` expected) actual) = Left $ UnexpectedFields expected actual (Record actual_bindings) (TypeRecord expected_bindings)
  | not (all (`elem` actual) expected) = Left $ MissingFields expected actual (Record actual_bindings) (TypeRecord expected_bindings)
  | otherwise = return ()
  where
    actual = [name | (ABinding name _) <- actual_bindings]
    expected = [name | (ARecordFieldType name _) <- expected_bindings]

--Checks well-formed types. Basicly just checks tht records and variants have unique names for fields or members
validateType :: Type -> TypeCheckerResult Type
validateType t@(TypeRecord fields) =
  case duplicateIn [name | (ARecordFieldType name _) <- fields] of
    Nothing -> do
      () <- sequence_ $ validateType <$> [t | (ARecordFieldType _ t) <- fields]
      return t
    Just duplicate -> Left $ DuplicateRecordFields duplicate t
validateType t@(TypeVariant members) =
  case duplicateIn [name | (AVariantFieldType name _) <- members] of
    Nothing -> do
      () <- sequence_ $ validateType <$> [t | (AVariantFieldType _ (SomeTyping t)) <- members]
      return t
    Just duplicate -> Left $ DuplicateRecordFields duplicate t
validateType t = Right t

-- Joins a contexts produced by several simultaneously matched patterns (e.g. in multivariable let or in PatternTuple)
joinPatternContexts :: [Context] -> TypeCheckerResult Context
joinPatternContexts contexts =
  let result = concat contexts
   in if haveNoDuplicates result then return result else Left DuplicateVariablePattern

-- Checks whether bunch of patterns covers all possible values of a type
-- Prerequisite: all patterns shall match the type (e.g. checked via patternContext function)
-- WIP: structural patterns exhaustiveness check
isExhaustive :: Type -> [Pattern] -> Bool
isExhaustive t@(TypeSum _ _) patterns =
  any (isIrrefutable t) patterns || (any isPatternForInl patterns && any isPatternForInr patterns)
  where
    isPatternForInl (PatternInl (PatternVar _)) = True
    isPatternForInl _ = False
    isPatternForInr (PatternInr (PatternVar _)) = True
    isPatternForInr _ = False
isExhaustive t@(TypeVariant members) patterns =
  any (isIrrefutable t) patterns || all (\member -> any (isPatternForVariantField member) patterns) members
  where
    isPatternForVariantField :: VariantFieldType -> Pattern -> Bool
    isPatternForVariantField (AVariantFieldType label (SomeTyping _)) (PatternVariant label' (SomePatternData (PatternVar _))) = label == label'
    isPatternForVariantField (AVariantFieldType label NoTyping) (PatternVariant label' NoPatternData) = label == label'
    isPatternForVariantField _ _ = False
isExhaustive _ _ = False

-- Checks whether pattern is irrefutable
-- Prerequisite: pattern shall match the type (e.g. checked via patternContext function)
-- WIP: structural patterns exhaustiveness check
isIrrefutable :: Type -> Pattern -> Bool
isIrrefutable _ (PatternVar _) = True
-- isIrrefutable (TypeTuple types) (PatternTuple pats) =
--  all (uncurry isIrrefutable) $ zip types pats
-- isIrrefutable (TypeRecord fiels) (PatternRecord pats) = False -- TODO
isIrrefutable _ _ = False

-- Produces the context which consists with binded names after matching expression of type `t`
-- against specific pattern
patternContext :: Pattern -> Type -> TypeCheckerResult Context
patternContext (PatternVar name) t = return [(name, t)]
patternContext (PatternInl pattern) (TypeSum l _) = patternContext pattern l
patternContext p@(PatternInl pattern) t = Left $ UnexpectedPatternForType p t
patternContext (PatternInr pattern) (TypeSum _ r) = patternContext pattern r
patternContext p@(PatternInr pattern) t = Left $ UnexpectedPatternForType p t
patternContext p@(PatternVariant label (SomePatternData inner)) t@(TypeVariant members) = do
  typing <- extractVariantMemberType label members
  case typing of
    NoTyping -> Left $ UnexpectedNullaryPattern label p t
    SomeTyping ty -> patternContext inner ty
patternContext p@(PatternVariant label NoPatternData) t@(TypeVariant members) = do
  typing <- extractVariantMemberType label members
  case typing of
    NoTyping -> return []
    SomeTyping ty -> Left $ UnexpectedNullaryPattern label p t
patternContext p@(PatternVariant label (SomePatternData inner)) t = Left $ UnexpectedPatternForType p t
patternContext p t = Left $ UnexpectedPatternForType p t

-- Just infer + patternContext for PatternBinding
patternBindingContext :: Context -> PatternBinding -> TypeCheckerResult Context
patternBindingContext ctx (APatternBinding pattern expr) = do
  t <- infer ctx expr
  patternContext pattern t

-- inference function: calculates type of expression based on its structure and context
-- Context contains information of externally defined variables types, with respect to possible shadowing
infer :: Context -> Expr -> TypeCheckerResult Type
infer _ ConstTrue = return TypeBool
infer _ ConstFalse = return TypeBool
infer ctx (If c t e) = do
  () <- ensure ctx c TypeBool
  lhs <- infer ctx t
  () <- ensure ctx e lhs
  return lhs
infer ctx (Abstraction params body) = do
  () <- sequence_ $ validateType <$> [t | (AParamDecl _ t) <- params]
  return_type <- infer ([(name, t) | (AParamDecl name t) <- params] ++ ctx) body
  return $ TypeFun [t | (AParamDecl name t) <- params] return_type
infer ctx e@(Application callee args) = do
  callee_type <- infer ctx callee
  case callee_type of
    (TypeFun params return_type) ->
      if length params == length args
        then do
          () <- sequence_ [ensure ctx a p | (p, a) <- zip params args]
          return return_type
        else Left $ MismatchedArgumentsNumber (length params) (length args) e
    _ -> Left $ NotAFunction callee
infer _ (ConstInt _) = return TypeNat
infer ctx (Succ num) = do
  () <- ensure ctx num TypeNat
  return TypeNat
infer ctx (IsZero num) = do
  () <- ensure ctx num TypeNat
  return TypeBool
infer ctx (NatRec num init step) = do
  () <- ensure ctx num TypeNat
  t <- infer ctx init
  () <- ensure ctx step (TypeFun [TypeNat] $ TypeFun [t] t)
  return t
infer ctx (Var name) = do
  case lookup name ctx of
    Nothing -> Left $ UndefinedVariable name
    (Just t) -> return t
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
  pattern_contexts <- sequence $ patternBindingContext ctx <$> bindings
  renewed_context <- joinPatternContexts pattern_contexts
  infer (renewed_context ++ ctx) body
infer ctx (TypeAsc e t) = do
  _ <- validateType t
  () <- ensure ctx e t
  return t
infer ctx (Inl e) = Left AmbigousSumType
infer ctx (Inr e) = Left AmbigousSumType
infer ctx (Match e []) = Left EmptyMatch
infer ctx (Match e cases@(c : cs)) = do
  t <- infer ctx e
  let patterns = [pattern | (AMatchCase pattern _) <- cases]
  branches_types <- sequence [patternContext pattern t >>= (\ctx' -> infer (ctx' ++ ctx) b) | (AMatchCase pattern b) <- cases]
  if not (isExhaustive t patterns)
    then Left $ NonexhaustivePatternMatching patterns e t
    else
      if allEqual branches_types
        then return $ head branches_types
        else Left MismatchedBranchesTypes
infer ctx (List []) = Left AmbigousListType
infer ctx (List exprs@(h : t)) = do
  exprs_types <- sequence $ infer ctx <$> exprs
  if allEqual exprs_types then return $ TypeList $ head exprs_types else Left MismatchedListTypes
infer ctx (ConsList head tail) = do
  t <- infer ctx head
  () <- ensure ctx tail (TypeList t)
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
infer ctx (Variant label (SomeExprData expr)) = Left AmbigousVariantType
infer ctx (Fix f) = do
  arrow <- infer ctx f
  case arrow of
    (TypeFun [arg] ret) ->
      if arg == ret
        then return arg
        else Left $ UnexpectedType f (TypeFun [ret] ret) arrow
    _ -> Left $ NotAFunction f
infer _ e = Left $ UnsupportedExpression e

-- verification function: ensures that expression can be typed with the specific type
-- Context contains information of externally defined variables types, with respect to possible shadowing
-- Default case is type inference and comparasion
ensure :: Context -> Expr -> Type -> TypeCheckerResult ()
ensure _ ConstTrue TypeBool = return ()
ensure _ ConstTrue expected = Left $ UnexpectedType ConstTrue expected TypeBool
ensure _ ConstFalse TypeBool = return ()
ensure _ ConstFalse expected = Left $ UnexpectedType ConstFalse expected TypeBool
ensure ctx (If c t e) expected = do
  () <- ensure ctx c TypeBool
  () <- ensure ctx t expected
  ensure ctx e expected
ensure ctx e@(Abstraction params body) (TypeFun expected_args return_type) = do
  let actual_args = [t | (AParamDecl name t) <- params]
  if length expected_args /= length actual_args
    then Left $ UnexpectedArgumentsNumberInLambda (length expected_args) (length actual_args) e
    else
      if actual_args == expected_args
        then ensure ([(name, t) | (AParamDecl name t) <- params] ++ ctx) body return_type
        else
          Left $
            UnexpectedTypeForParameter
              expected_args
              actual_args
ensure ctx e@(Abstraction _ _) t = Left $ UnexpectedLambda e t
ensure _ (ConstInt _) TypeNat = return ()
ensure _ e@(ConstInt _) expected = Left $ UnexpectedType e expected TypeNat
ensure ctx (NatRec num init step) expected = do
  () <- ensure ctx num TypeNat
  () <- ensure ctx init expected
  ensure ctx step (TypeFun [TypeNat] $ TypeFun [expected] expected)
ensure _ ConstUnit TypeUnit = return ()
ensure _ ConstUnit expected = Left $ UnexpectedType ConstUnit expected TypeUnit
ensure ctx t@(Tuple elems) e@(TypeTuple tuple_elems)
  | length elems /= length tuple_elems = Left $ UnexpectedTupleLength (length tuple_elems) (length elems) e t
  | otherwise = sequence_ [ensure ctx e t | (e, t) <- zip elems tuple_elems]
ensure ctx e@(Tuple elems) t = Left $ UnexpectedTuple e t
ensure ctx (Record bindings) t@(TypeRecord fields) = do
  _ <- validateType t
  () <- suitsRecordType bindings fields
  sequence_ [extractRecordFieldType name fields >>= ensure ctx e | (ABinding name e) <- bindings]
ensure ctx e@(Record _) t = Left $ UnexpectedRecord e t
ensure ctx (Inl e) (TypeSum l _) = ensure ctx e l
ensure ctx e@(Inl _) t = Left $ UnexpectedInjection e t
ensure ctx (Inr e) (TypeSum _ r) = ensure ctx e r
ensure ctx e@(Inr _) t = Left $ UnexpectedInjection e t
ensure ctx (Match e cases@(c : cs)) expected = do
  t <- infer ctx e
  let patterns = [pattern | (AMatchCase pattern _) <- cases]
  if not (isExhaustive t patterns)
    then Left $ NonexhaustivePatternMatching patterns e t
    else sequence_ [patternContext pattern t >>= (\ctx' -> ensure (ctx' ++ ctx) b expected) | (AMatchCase pattern b) <- cases]
ensure ctx (List []) (TypeList _) = return ()
ensure ctx (List exprs) (TypeList expected) = sequence_ $ [ensure ctx expr expected | expr <- exprs]
ensure ctx e@(List _) t = Left $ UnexpectedList e t
ensure ctx (ConsList head tail) (TypeList expected) = do
  () <- ensure ctx head expected
  ensure ctx tail expected
ensure ctx e@(ConsList head tail) t = Left $ UnexpectedList e t
ensure ctx (Head list) expected = ensure ctx list (TypeList expected)
ensure ctx (Tail list) expected@(TypeList _) = ensure ctx list expected
ensure ctx e@(Tail list) t = Left $ UnexpectedList e t
ensure ctx e@(Variant label (SomeExprData expr)) t@(TypeVariant members) = do
  expected <- extractVariantMemberType label members
  case expected of
    NoTyping -> Left $ UnexpectedDataForNullaryType label e t
    SomeTyping ty -> ensure ctx expr ty
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
ensure ctx expr expected = do
  infered <- infer ctx expr
  if expected == infered
    then return ()
    else Left $ UnexpectedType expr expected infered

-- validate target + ensure
validate'n'ensure :: Context -> Expr -> Type -> TypeCheckerResult ()
validate'n'ensure ctx e t = do
  t' <- validateType t
  -- () <- sequence_ $ validateType . snd <$> ctx
  ensure ctx e t'

-- validate context + validate target + ensure
validate'n'ensure'ctx :: Context -> Expr -> Type -> TypeCheckerResult ()
validate'n'ensure'ctx ctx e t = do
  () <- sequence_ $ validateType . snd <$> ctx
  validate'n'ensure ctx e t
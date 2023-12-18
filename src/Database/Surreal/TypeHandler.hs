{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Database.Surreal.TypeHandler where

import           ClassyPrelude        as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Foldable        hiding ( elem, notElem )
import           Data.Row
import           Database.Surreal.AST as AST
import qualified Language.Haskell.TH  as TH

getFieldLabel :: MonadFail m => Field -> m Text
getFieldLabel = \case
  WildCardField -> fail "Can't determine field label for WildCardField '*'!"
  SimpleField t -> return t
  IndexedField f _ -> getFieldLabel f
  FilteredField f _ -> getFieldLabel f
  FieldParam p -> case p of
    SQLParam t -> return t
    InputParam t _ -> return t
  CompositeField f1 f2 -> do
    l1 <- getFieldLabel f1
    l2 <- getFieldLabel f2
    return $ l1 <> l2

getEdgeLabel :: MonadFail m => Edge -> m Text
getEdgeLabel = \case
  OutEdge f -> ("->" <>) <$> getFieldLabel f
  InEdge f -> ("<-" <>) <$>  getFieldLabel f

extractSelectorTypes :: MonadFail m => Selectors -> m [(Maybe Text, TypeDef)]
extractSelectorTypes (Selectors ss) = mapM (\s -> do
                                               t <- getSelectorType s
                                               l <- getSelectorLabel s
                                               return (Just l, t))
                                      ss
  where
    getSelectorType :: MonadFail m => Selector -> m TypeDef
    getSelectorType = \case
      TypedSelector _ t -> return t
      FieldSelector (FieldParam (InputParam _ t)) -> return t
      ExpSelector _ (FieldParam (InputParam _ t)) -> return t
      SelectorAs _ (FieldParam (InputParam _ t)) -> return t
      SelectorAs s _ -> getSelectorType s
      a -> fail $ "No type definition given for: " <> show a
    getSelectorLabel :: MonadFail m => Selector -> m Text
    getSelectorLabel = \case
      FieldSelector f -> getFieldLabel f
      ExpSelector _ f -> getFieldLabel f
      SelectorAs _ f -> getFieldLabel f
      EdgeSelector _ _ f -> getFieldLabel f
      TypedSelector s _ -> getSelectorLabel s

-- | Gives the base type for the select expression, after applying the clauses that
-- modiify the return type like omit, value, split, etc
getSelectBaseType :: MonadFail m => Exp -> m [(Maybe Text, TypeDef)]
getSelectBaseType = \case
  SelectE mValue selectors mOmit _ _ mSplit _ _ _ _ _ _ _ mExplain -> do
    baseTypes <- extractSelectorTypes selectors
    applyValue baseTypes >>= applyOmit >>= applySplit >>= applyExplain
    where
      -- | if VALUE is given, only the first type will be returned in an array without a label
      applyValue :: MonadFail m => [(Maybe Text, TypeDef)] -> m [(Maybe Text, TypeDef)]
      applyValue ts = case (mValue, ts) of
        (Just _,(_ , t) : _) -> return [(Nothing, t)]
        (Just _, a) -> fail $ "Expected at least one type but got: " <> show a
        (Nothing, ts') -> return ts'
      -- | if OMIT xs is given, the fields in the omit clause are removed from results
      applyOmit :: MonadFail m => [(Maybe Text, TypeDef)] -> m [(Maybe Text, TypeDef)]
      applyOmit ts = case mOmit of
        Just (OMIT fs) -> do
          fieldLabels <- mapM (fmap Just . getFieldLabel) fs
          return $ filter (\(ls, _) -> do
                              ls `notElem` fieldLabels)
                            ts
        Nothing -> return ts
      -- | if SPLIT xs is given, the fields in the split clause are turned
      -- from array/vector type into single value type
      applySplit :: MonadFail m => [(Maybe Text, TypeDef)] -> m [(Maybe Text, TypeDef)]
      applySplit ts = case mSplit of
        Just (SPLIT fs) -> do
          fieldLabels <- mapM (fmap Just . getFieldLabel) fs
          return $ foldl (\r initialT@(ls, t) ->
                            if ls `elem` fieldLabels
                            then case t of
                              T "Vector" [t'] -> (ls, t') : r
                              a -> fail $ "Expected a Vector type (Vector a) but got: " <> show a
                            else initialT : r)
                      [] ts
        Nothing -> return ts
      -- | if EXPLAIN or EXPLAIN FULL is given, the execution plan of the query is returned instead
      applyExplain :: MonadFail m => [(Maybe Text, TypeDef)] -> m [(Maybe Text, TypeDef)]
      applyExplain ts = case mExplain of
        Just _ -> return [(Just "detail", T "Value" []), (Just "operation", T "Value" [])]
        Nothing  -> return ts
  a -> fail $ "Expected a Select expression but got: " <> show a

getBaseType :: MonadFail m => Exp -> m [(Maybe Text, TypeDef)]
getBaseType = \case
  TypedE _ t -> return [(Nothing, t)]
  ReturnE e -> getBaseType e
  se@(SelectE {}) -> getSelectBaseType se
  ie@(InsertE {}) -> fail $ "getBaseType: Insert Expressions must have a type: " <> show ie
  a -> fail $ "getBaseType: undefined: " <> show a

-- | converts `TypeDef` to TH type definition AST
mkType :: TypeDef -> TH.Type
mkType (T name params) = case reverse params of
  []   -> TH.ConT (TH.mkName name)
  t:ts -> TH.AppT (prepend (TH.ConT (TH.mkName name)) (reverse ts)) (mkType t)
  where
    prepend t ts = case reverse ts of
      []     -> t
      t':ts' -> TH.AppT (prepend t (reverse ts')) (mkType t')

-- | returns a row type like `"name" .== Text`
getRowType :: MonadFail m => (Maybe Text, TypeDef) -> m TH.Type
getRowType = \case
  (Just l, t) -> do
    let t' = mkType t
    return
      $ TH.InfixT (TH.LitT (TH.StrTyLit $ unpack l)) ''(.==) t'
  a -> fail $ "getRowType: Can't make a row type without a label: " <> show a

-- | returns a row type like `"name" .== Text`
getRowTypeFromInput :: MonadFail m => Param -> m TH.Type
getRowTypeFromInput = \case
  InputParam l t -> do
    let t' = mkType t
    return
      $ TH.InfixT (TH.LitT (TH.StrTyLit $ unpack l)) ''(.==) t'
  a -> fail $ "getRowTypeFromInput: Can't make a row type without a label: " <> show a

combineToRowType :: TH.Type -> TH.Type -> TH.Type
combineToRowType t1 t2 = TH.InfixT t2 ''(.+) t1

mkRecType :: MonadFail m => [(Maybe Text, TypeDef)] -> m TH.Type
mkRecType types = do
  rTypes <- mapM getRowType types
  return $ foldr1 combineToRowType rTypes

mkRecTypeFromInputs :: MonadFail m => [Param] -> m TH.Type
mkRecTypeFromInputs params = do
  rTypes <- mapM getRowTypeFromInput params
  return $ foldr1 combineToRowType rTypes

getExpressionType :: MonadFail m => Exp -> m TH.Type
getExpressionType e = do
  t <- getBaseType e
  case t of
    []              -> return $ TH.TupleT 0
    [(Nothing, t')] -> return $ mkType t'
    ts              -> mkRecType ts

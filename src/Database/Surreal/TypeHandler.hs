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

import           ClassyPrelude              as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Control.Monad.State.Strict
import           Data.Foldable              hiding ( elem, notElem )
import qualified Data.Vector                as V
import           Database.Surreal.AST
import qualified Language.Haskell.TH        as TH

getFieldLabel :: Field -> Text
getFieldLabel = \case
  SimpleField t -> t
  IndexedField f _ -> getFieldLabel f
  FilteredField f _ -> getFieldLabel f
  CompositeField f1 f2 -> getFieldLabel f1 <> getFieldLabel f2

getEdgeLabel :: Edge -> Text
getEdgeLabel = \case
  OutEdge f -> "->" <> getFieldLabel f
  InEdge f -> "<-" <> getFieldLabel f

extractSelectorTypes :: MonadFail m => Selectors -> m [(Maybe Text, TypeDef)]
extractSelectorTypes (Selectors ss) = mapM (\s -> do
                                               t <- getSelectorType s
                                               let l = getSelectorLabel s
                                               return (Just l, t))
                                      ss
  where
    getSelectorType :: MonadFail m => Selector -> m TypeDef
    getSelectorType = \case
      TypedSelector _ t -> return t
      a -> fail $ "No type definition given for: " <> show a
    getSelectorLabel :: Selector -> Text
    getSelectorLabel = \case
      FieldSelector f -> getFieldLabel f
      ExpSelector _ f -> getFieldLabel f
      SelectorAs _ f -> getFieldLabel f
      EdgeSelector _ _ f -> getFieldLabel f
      TypedSelector s _ -> getSelectorLabel s

-- | Gives the base type for the select expression, after applying the clauses that
-- modiify the return type like omit, value, group by, etc
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
          let fieldLabels = map (Just . getFieldLabel) fs
          return $ filter (\(ls, _) -> do
                              ls `notElem` fieldLabels)
                            ts
        Nothing -> return ts
      -- | if SPLIT xs is given, the fields in the split clause are turned
      -- from array/vector type into single value type
      applySplit :: MonadFail m => [(Maybe Text, TypeDef)] -> m [(Maybe Text, TypeDef)]
      applySplit ts = case mSplit of
        Just (SPLIT fs) -> do
          let fieldLabels = map (Just . getFieldLabel) fs
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

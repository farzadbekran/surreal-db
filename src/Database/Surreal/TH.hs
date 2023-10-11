{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Database.Surreal.TH where

import           ClassyPrelude                   as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Aeson as Aeson
import           Data.Row.Records
import qualified Database.Surreal.AST            as AST
import           Database.Surreal.Parser
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec                 hiding ( Label )
import Data.Foldable
import qualified Data.Vector as V

data Encoder a
  = Encoder (a -> Value)
  | EmptyEncoder
newtype Decoder a
  = Decoder (Value -> Surreal a)

newtype DecodeError = DecodeError String
  deriving (Show, Exception)

reEncode :: (b -> a) -> Encoder a -> Encoder b
reEncode f (Encoder a)  = Encoder (a . f)
reEncode _ EmptyEncoder = EmptyEncoder

reDecode :: (a -> Surreal b) -> Decoder a -> Decoder b
reDecode f (Decoder a) = Decoder (\r -> do
                                     r1 <- a r
                                     f r1)

-- | The type used by TH to parse SurrealQL
data Query input output
  = Query Text (Encoder input) (Decoder output)

-- instance Bifunctor Query where
--   bimap :: (a -> b) -> (c -> d) -> Query a c -> Query b d
--   bimap l r (Query t a c) = Query t (l a) (r c)

query :: QuasiQuoter
query = QuasiQuoter
  { quoteDec  = P.error "quoteDec not supported in query"
  , quoteExp  = parseQuery
  , quotePat  = P.error "quotePat not supported in query"
  , quoteType = P.error "quoteType not supported in query"
  }

combineToRowTypeDef :: Type -> Type -> Type
combineToRowTypeDef t1 t2 = InfixT t2 ''(.+) t1

getResultDecoders :: AST.Exp -> Q Exp
getResultDecoders = \case
  AST.SelectE _ (AST.Selectors ss) _ _ _ _ _ _ _ _ _ _ _ _ -> do
    types <- mapM getSelectorRowType ss
    let rowType = foldr1 combineToRowTypeDef types
    [| Decoder (\case
                   Array o -> (\case
                                  (Object r) -> case r !? "result" of
                                    Just r1 -> do
                                      case fromJSON @[Rec $(return rowType)] r1 of
                                        Aeson.Success r2 -> return r2
                                        Aeson.Error e -> P.throwIO $ DecodeError e
                                    Nothing -> P.throwIO $ DecodeError "Select Decoder: missing result key in object!"
                                  v -> P.throwIO $ DecodeError $ "Select Decoder: Unexpected result format: " <> show v)
                              (V.head o)
                   v -> P.throwIO $ DecodeError $ "Select Decoder: Unexpected result format: expecting array but got: " <> (pack $ show v)
               ) |]
  _ -> fail "unimplemented decoder!"

-- | converts `AST.TypeDef` to TH type definition AST
mkType :: AST.TypeDef -> Type
mkType (AST.T name params) = case reverse params of
  []   -> ConT (mkName name)
  t:ts -> AppT (prepend (ConT (mkName name)) (reverse ts)) (mkType t)
  where
    prepend t ts = case reverse ts of
      []     -> t
      t':ts' -> AppT (prepend t (reverse ts')) (mkType t')

-- | returns a row type like `"name" .== Text`
getSelectorRowType :: AST.Selector -> Q Type
getSelectorRowType s = case s of
  AST.TypedSelector _ _ -> do
    t <- getSelectorType s
    return
      $ InfixT (LitT (StrTyLit $ getSelectorLabel s)) ''(.==) t
  a -> P.error $ "getSelectorRowType: No type information provided for selector: " <> show a

getSelectorType :: AST.Selector -> Q Type
getSelectorType = \case
  AST.TypedSelector _ t -> return $ mkType t
  a -> P.error $ "getSelectorType: No type information provided for selector: " <> show a

getSelectorLabel :: AST.Selector -> String
getSelectorLabel = \case
  AST.FieldSelector (AST.Field t) -> unpack t
  AST.ExpSelector _ (AST.Field t) -> unpack t
  AST.FieldSelectorAs (AST.Field t) _ -> unpack t
  AST.TypedSelector s _ -> getSelectorLabel s

parseQuery :: String -> Q Exp
parseQuery s = do
  let eAST = parse exp "" s
  case eAST of
    Right ast -> [| Query $(lift $ AST.toQL ast) EmptyEncoder $(getResultDecoders ast) |]
    Left e    -> fail $ errorBundlePretty e

runQuery :: input -> Query input output -> Surreal output
runQuery _ (Query q _ (Decoder decoder)) = do
  RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [String q]
  --print result
  case err of
    Just e  -> P.throwIO e
    Nothing -> decoder $ fromMaybe Null result

-- signIn :: MonadUnliftIO m => Text -> Text -> m (Either RPC.Error ())
-- signIn user pass = do
--   RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [q]
--   case err of
--     Just e  -> return $ Left e
--     Nothing -> return $ Right $ f $ fromMaybe [] result

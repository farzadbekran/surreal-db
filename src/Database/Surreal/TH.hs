{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Database.Surreal.TH where

import           ClassyPrelude                   as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Aeson
import qualified Database.Surreal.AST            as AST
import           Database.Surreal.Parser
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec

data Encoder a
  = Encoder (a -> Value)
  | EmptyEncoder
newtype Decoder a = Decoder (Value -> a)

reEncode :: (b -> a) -> Encoder a -> Encoder b
reEncode f (Encoder a) = Encoder (a . f)
reEncode _ EmptyEncoder = EmptyEncoder

reDecode :: (a -> b) -> Decoder a -> Decoder b
reDecode f (Decoder a) = Decoder (f . a)

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

-- getResultDecoders :: AST.Exp -> Q Exp
-- getResultDecoders = \case
--   AST.SelectE _ (AST.Selectors ss) _ _ _ _ _ _ _ _ _ _ _ _ -> do
--     let types = getSelectorTypes ss
--     return $ linkDecoders $ map mkDecoder types
--   _ -> fail "unimplemented decoder!"
--   where
--     mkDecoder t = AppE (UnboundVarE 'Decoder) (AppTypeE (VarE fromJSON) t)
--     linkDecoders ds = 

-- getSelectorTypes :: [AST.Selector] -> [Type]
-- getSelectorTypes = map getType
--   where
--     getType = \case
--       AST.TypedSelector _ t -> ConT $ mkName $ unpack t
--       _ -> TupleT 0

parseQuery :: String -> Q Exp
parseQuery s = do
  let eAST = parse exp "" s
  case eAST of
    Right ast -> [| Query $(lift $ AST.toQL ast) EmptyEncoder (Decoder P.id) |]
    Left e    -> fail $ errorBundlePretty e

runQuery :: input -> Query input output -> Surreal (Either RPC.Error output)
runQuery _ (Query q _ (Decoder decoder)) = do
  RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [String q]
  case err of
    Just e  -> return $ Left e
    Nothing -> return $ Right $ decoder $ fromMaybe Null result

-- signIn :: MonadUnliftIO m => Text -> Text -> m (Either RPC.Error ())
-- signIn user pass = do
--   RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [q]
--   case err of
--     Just e  -> return $ Left e
--     Nothing -> return $ Right $ f $ fromMaybe [] result

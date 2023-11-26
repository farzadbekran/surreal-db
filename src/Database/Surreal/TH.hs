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

module Database.Surreal.TH where

import           ClassyPrelude                   as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Aeson                      as Aeson
import           Data.Foldable
import           Data.Row.Records
import qualified Data.Vector                     as V
import qualified Database.Surreal.AST            as AST
import           Database.Surreal.Parser
import           Database.Surreal.TypeHandler
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec                 hiding ( Label )

data Encoder a
  = Encoder (a -> Value)
  | EmptyEncoder
newtype Decoder a
  = Decoder (Value -> Surreal a)

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

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

getResultDecoders :: AST.Exp -> Q Exp
getResultDecoders e = do
  t <- getExpressionType e
  case e of
    AST.SelectE {} -> do
      [| Decoder (\case
                     Array o -> (\case
                                    (Object r) -> case r !? "result" of
                                      Just r1 -> do
                                        case fromJSON @[Rec $(return t)] r1 of
                                          Aeson.Success r2 -> return r2
                                          Aeson.Error err -> P.throwIO $ DecodeError err
                                      Nothing -> P.throwIO $ DecodeError "Select Decoder: missing result key in object!"
                                    v -> P.throwIO $ DecodeError $ "Select Decoder: Unexpected result format: " <> show v)
                                (V.head o)
                     v -> P.throwIO $ DecodeError $ "Select Decoder: Unexpected result format: expecting array but got: " <> pack (show v)
                 ) |]
    _ -> fail "unimplemented decoder!"

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

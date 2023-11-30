{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Database.Surreal.TH where

import           ClassyPrelude                   as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Aeson                      as Aeson
import           Data.Foldable                   hiding ( concatMap )
import           Data.Row.Records
import           Data.Type.Equality
import qualified Data.Vector                     as V
import           Database.Surreal.AST            ( HasInput (getInputs) )
import qualified Database.Surreal.AST            as AST
import           Database.Surreal.Parser
import           Database.Surreal.TypeHandler
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec                 hiding ( Label )

-- | The type used by TH to parse SurrealQL
data Query input output
  = Query Text (input -> Value) (Value -> Surreal output)

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

reEncode :: (i2 -> i) -> Query i o -> Query i2 o
reEncode f (Query t ie od)  = Query t (ie . f) od

reDecode :: (o -> Surreal o2) -> Query i o -> Query i o2
reDecode f (Query t ie od)
  = Query t ie (od >=> f)

sql :: QuasiQuoter
sql = QuasiQuoter
  { quoteDec  = P.error "quoteDec not supported in query"
  , quoteExp  = parseSQL
  , quotePat  = P.error "quotePat not supported in query"
  , quoteType = P.error "quoteType not supported in query"
  }

getExpResultDecoder :: AST.Exp -> Q Exp
getExpResultDecoder e = do
  t <- getExpressionType e
  case e of
    AST.SelectE {} -> do
      [| (\case
             (Object r) -> case r !? "result" of
                             Just r1 -> do
                               case fromJSON @[Rec $(return t)] r1 of
                                 Aeson.Success r2 -> return r2
                                 Aeson.Error err  -> P.throwIO $ DecodeError err
                             Nothing -> P.throwIO $ DecodeError "Select Decoder: missing 'result' key in object!"
             v -> P.throwIO $ DecodeError $ "Select Decoder: Unexpected result format: " <> show v)
       |]
    _ -> fail "unimplemented decoder!"

-- TODO: add support for return statement
getLineResultDecoder :: AST.SurQLLine -> Q Exp
getLineResultDecoder = \case
  AST.ExpLine e -> getExpResultDecoder e
  AST.StatementLine _ -> [| (\_ -> return ()) |]

-- | we only care about the type of the last line, and decode that
getBlockResultDecoders :: AST.Block -> Q Exp
getBlockResultDecoders (AST.Block ls) = do
  decoders <- mapM getLineResultDecoder ls
  case lastMay decoders of
    Just decoder ->
      [| (\case
           Array a -> $(return decoder) (V.last a)
           v -> P.throwIO $ DecodeError
             $ "Base Decoder: Unexpected result format: expecting array but got: "
             <> pack (show v)
         ) |]
    Nothing -> fail $ "No decoder for the last line in the SQL Block: " <> show ls

parseSQL :: String -> Q Exp
parseSQL s = do
  let blockAST = parse block "" s
  case blockAST of
    Right ast -> do
      let inputs = sortBy (compare `on` (\(AST.Input i _) -> i)) $ getInputs ast
      [| Query $(lift $ AST.toQL ast) $(getInputEncoder inputs) $(getBlockResultDecoders ast) |]
    Left e    -> fail $ errorBundlePretty e
  where
    getInputEncoder :: [AST.Input] -> Q Exp
    getInputEncoder inputs = lamE [getPatternBlock inputs] [| toJSON p |]
    getPatternBlock :: [AST.Input] -> Q Pat
    getPatternBlock inputs = do
      asP (mkName "p") (getSignatures inputs)
    getSignatures :: [AST.Input] -> Q Pat
    getSignatures [AST.Input _ t] = sigP wildP (return $ mkType t)
    getSignatures [] = sigP wildP (tupleT 0)
    getSignatures inputs = tupP $ P.map (\(AST.Input _ t) -> sigP wildP (return $ mkType t)) inputs


-- TODO: apply inputs here
runQuery :: input -> Query input output -> Surreal output
runQuery _ (Query q _ decoder) = do
  RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [String q]
  --print result
  case err of
    Just e  -> P.throwIO e
    Nothing -> decoder $ fromMaybe Null result

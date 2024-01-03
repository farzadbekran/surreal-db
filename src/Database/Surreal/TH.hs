{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Database.Surreal.TH
    ( Query
    , getDecoder
    , getEncoder
    , getSQL
    , sql
    ) where

import           ClassyPrelude                as P hiding ( exp, lift )
import           Control.Monad.Fail
import           Data.Aeson                   as Aeson
import           Data.Profunctor
import           Data.Row.Records
import           Database.Surreal.AST         ( HasInput (getInputs) )
import qualified Database.Surreal.AST         as AST
import           Database.Surreal.Parser
import           Database.Surreal.TypeHandler
import           Database.Surreal.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec              hiding ( Label )

-- | The type used by TH to parse SurrealQL
data Query input output
  = Query Text (input -> Value) (Value -> output)

getSQL :: Query input output -> Text
getSQL (Query t _ _) = t

getEncoder :: Query input output -> (input -> Value)
getEncoder (Query _ encoder _) = encoder

getDecoder :: Query input output -> (Value -> output)
getDecoder (Query _ _ decoder) = decoder

instance Profunctor Query where
  dimap :: (a -> b) -> (c -> d) -> Query b c -> Query a d
  dimap ab cd (Query t b c) = Query t (b . ab) (cd . c)

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
      [| (\r -> case fromJSON @[Rec $(return t)] r of
             Aeson.Success r2 -> Right r2
             Aeson.Error err  -> Left $ DecodeError err)
       |]
    AST.TypedE {} -> do
      [| (\r -> case fromJSON @($(return t)) r of
             Aeson.Success r2 -> Right r2
             Aeson.Error err  -> Left $ DecodeError err)
       |]
    AST.ReturnE {} -> do
      [| (\r -> case fromJSON @($(return t)) r of
             Aeson.Success r2 -> Right r2
             Aeson.Error err  -> Left $ DecodeError err)
       |]
    AST.LiveSelectE {} -> [| (\r -> case fromJSON @($(return t)) r of
                                 Aeson.Success r2 -> Right r2
                                 Aeson.Error err  -> Left $ DecodeError err) |]
    _ -> [| (\r -> case fromJSON @Value r of
                Aeson.Success r2 -> Right r2
                Aeson.Error err  -> Left $ DecodeError err) |]

getLineResultDecoder :: AST.SurQLLine -> Q Exp
getLineResultDecoder = \case
  AST.ExpLine e -> getExpResultDecoder e
  AST.StatementLine _ -> [| (\_ -> return ()) |]

-- | we only care about the type of the last line, and decode that
getBlockResultDecoders :: AST.Block -> Q Exp
getBlockResultDecoders (AST.Block ls) = do
  decoders <- mapM getLineResultDecoder ls
  case lastMay decoders of
    Just decoder -> [| $(return decoder) |]
    Nothing -> fail $ "No decoder for the last line in the SQL Block: " <> show ls

parseSQL :: String -> Q Exp
parseSQL s = do
  let blockAST = parse blockStrict "" s
  case blockAST of
    Right ast -> do
      let inputs = getInputs ast
      [| Query $(lift $ AST.toQL ast) $(getInputEncoder inputs) $(getBlockResultDecoders ast) |]
    Left e    -> fail $ errorBundlePretty e
  where
    getInputEncoder :: [AST.Param] -> Q Exp
    getInputEncoder [] = [| (\_ -> object [] ) |]
    getInputEncoder inputs = [| toJSON :: Rec $(mkRecTypeFromInputs inputs) -> Value |]

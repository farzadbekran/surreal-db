{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NamedFieldPuns #-}

module Database.Surreal.TH where

import           ClassyPrelude                   as P hiding ( exp, lift )
import           Control.Monad.Fail
import qualified Database.Surreal.AST            as AST
import           Database.Surreal.Parser
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec

-- | The type used by TH to parse SurrealQL
newtype Query input output
  = Query Text
  deriving (Show)

query :: QuasiQuoter
query = QuasiQuoter
  { quoteDec  = P.error "quoteDec not supported in query"
  , quoteExp  = parseQuery
  , quotePat  = P.error "quotePat not supported in query"
  , quoteType = P.error "quoteType not supported in query"
  }

parseQuery :: String -> Q Exp
parseQuery s = do
  let eAST = parse exp "" s
  case eAST of
    Right ast -> [| Query $(lift $ AST.toQL ast) :: Query () [RPC.QueryResult] |]
    Left e    -> fail $ errorBundlePretty e

runQuery :: MonadUnliftIO m => input -> Query input output -> ([RPC.QueryResult] -> output ) -> m (Either RPC.Error output)
runQuery _ (Query q) f = do
  RPC.Response { RPC.result = result, RPC.error = err } <- RPC.send "query" [q]
  case err of
    Just e  -> return $ Left e
    Nothing -> return $ Right $ f $ fromMaybe [] result

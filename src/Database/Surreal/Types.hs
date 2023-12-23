{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Database.Surreal.Types where

import           ClassyPrelude hiding ( error, id )
import           Control.Monad ( MonadFail )
import           Data.Aeson    as J

class MonadFail m => MonadSurreal m where
  getNextRequestID :: m Int
  send :: Text -> [Value] -> m Response
  runQuery :: input -> Query input (Either DecodeError output) -> m output

data Request
  = Request
      { id     :: Int
      , method :: Text
      , params :: [Value]
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data Response
  = Response
      { id     :: Int
      , result :: Maybe Value
      , error  :: Maybe SurrealError
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data QueryResult
  = QueryResult
      { result :: Maybe Value
      , status :: Text
      , time   :: Text
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data SurrealError
  = SurrealError
      { code    :: Int
      , message :: Text
      }
  deriving (Eq, Exception, FromJSON, Generic, Read, Show, ToJSON)

data ConnectionInfo
  = ConnectionInfo
      { url  :: String
      , port :: Int
      , user :: Text
      , pass :: Text
      , ns   :: Text
      , db   :: Text
      }
  deriving (Eq, Read, Show)

defaultConnectionInfo :: ConnectionInfo
defaultConnectionInfo = ConnectionInfo "0.0.0.0" 8000 "root" "root" "test" "test"

-- | The type used by TH to parse SurrealQL
data Query input output
  = Query Text (input -> Value) (Value -> output)

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

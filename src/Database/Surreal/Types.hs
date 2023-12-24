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

import           ClassyPrelude   hiding ( error, id )
import           Control.Monad   ( MonadFail )
import           Data.Aeson      as J
import           Data.Profunctor

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

data LiveAction = CREATE | UPDATE | DELETE
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data LiveResponse
  = LiveResponse
      { id     :: Text
      , result :: Value
      , action :: LiveAction
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

newtype LiveNotification = LiveNotification { result :: LiveResponse }
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

instance Profunctor Query where
  dimap :: (a -> b) -> (c -> d) -> Query b c -> Query a d
  dimap ab cd (Query t b c) = Query t (b . ab) (cd . c)

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

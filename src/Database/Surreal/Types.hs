{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}

module Database.Surreal.Types where

import           ClassyPrelude       hiding ( error, id )
import           Data.Aeson          as J

data Request
  = Request
      { id     :: !Int
      , method :: !Text
      , params :: ![Value]
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

newtype LiveNotification
  = LiveNotification { result :: LiveResponse }
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

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

newtype DriverError
  = DriverError String
  deriving (Exception, Show)

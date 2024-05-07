{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Surreal.Effect.Handlers.RPCTypes where

import           ClassyPrelude          hiding ( error, id )
import           Control.Concurrent     ( ThreadId )
import           Database.Surreal.Types
import qualified Network.WebSockets     as WS

data RPCConnectionState
  = RPCConnectionState
      { conn           :: !WS.Connection
      , nextReqID      :: !(TVar Int)
      , respMap        :: !(TVar (Map Int (MVar Response)))
      , liveRespMap    :: !(TVar (Map Text (LiveResponse -> IO ())))
      , listenerThread :: !ThreadId
      }

data RPCError
  = NotConnected
  | InvalidResponse !Text
  | RequestTimeout !Text
  | SigninError !(Maybe SurrealError)
  | SurrealErr !SurrealError
  | DecodeErr !DecodeError
  | DriverErr !DriverError
  | IOException !SomeException
  deriving (Exception, Show)

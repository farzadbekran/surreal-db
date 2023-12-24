{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Surreal.WS.RPCTypes where

import           ClassyPrelude          hiding ( error, id )
import           Control.Concurrent     ( ThreadId )
import           Database.Surreal.Types
import qualified Network.WebSockets     as WS

data RPCConnectionState
  = RPCConnectionState
      { conn           :: WS.Connection
      , nextReqID      :: TVar Int
      , respMap        :: TVar (Map Int (MVar Response))
      , liveRespMap    :: TVar (Map Text (LiveResponse -> IO ()))
      , listenerThread :: ThreadId
      }

data RPCNetworkException
  = NotConnected
  | InvalidResponse Text
  | RequestTimeout Request
  | SigninError (Maybe SurrealError)
  deriving (Exception, Show)

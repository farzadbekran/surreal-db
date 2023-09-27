{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Surreal.WS.RPC.Surreal where

import           ClassyPrelude        hiding ( error, id )
import           Control.Concurrent   ( ThreadId, forkIO, threadDelay )
import           Control.Exception    ( throw )
import           Data.Aeson           ( FromJSON, ToJSON, Value, decode,
                                        encode )
import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict      as M
import           Network.Socket       ( withSocketsDo )
import qualified Network.WebSockets   as WS
import Control.Monad.Except (MonadError)

type Surreal a = ReaderT ConnectionState IO a

data ConnectionState
  = ConnectionState
      { conn           :: WS.Connection
      , nextReqID      :: TVar Int
      , respMap        :: TVar (Map Int (MVar Response))
      , listenerThread :: ThreadId
      }

data NetworkException
  = NotConnected
  | InvalidResponse Text
  | RequestTimeout Request
  deriving (Exception, Show)

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
      , error  :: Maybe Error
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

-- data QueryResult
--   = QueryResult
--       { result :: Maybe Value
--       , status :: Text
--       , time   :: Text
--       }
--   deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data Error
  = Error
      { code    :: Int
      , message :: Text
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

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

{- |
starts a thread to listen to the connection and write responses to the
response map. return the threds ID
-}
app :: MVar WS.Connection -> TVar (Map Int (MVar Response)) -> WS.ClientApp ThreadId
app connMvar respMap connection = do
  putMVar connMvar connection
  forkIO $ forever $ do
    msg <- WS.receiveData connection
    let rsp = decode msg :: Maybe Response
    case rsp of
      Just r@Response { .. } -> do
        mvarMap <- readTVarIO respMap
        case mvarMap !? id of
          Just mvar -> putMVar mvar r
          Nothing   -> putStrLn "app: Missing MVar!"
      Nothing -> putStrLn $ "app: Invalid Response: " <> decodeUtf8 (BL.toStrict msg)

connect :: MonadIO m => ConnectionInfo -> m ConnectionState
connect ConnectionInfo { .. } = do
  connMVar <- newEmptyMVar
  respTVar <- newTVarIO M.empty
  tid <- liftIO $ withSocketsDo $ WS.runClient url port "/rpc" (app connMVar respTVar)
  conn <- readMVar connMVar
  reqIDTvar <- newTVarIO 0
  let connectionState = ConnectionState conn reqIDTvar respTVar tid
  -- TODO: signin before returning
  return connectionState

getNextRequestID :: Surreal Int
getNextRequestID = do
  ConnectionState { .. } <- ask
  atomically $ do
    nextID <- readTVar nextReqID
    writeTVar nextReqID $ nextID + 1
    return nextID

send :: Text -> [Value] -> Surreal Response
send method params = do
  ConnectionState { .. } <- ask
  nextID <- getNextRequestID
  mvar <- newEmptyMVar
  atomically $ modifyTVar' respMap (M.insert nextID mvar)
  let req = Request nextID method params
  liftIO $ WS.sendTextData conn $ encode req
  -- recover after 10 seconds of delay
  res <- race
    (do
      liftIO $ threadDelay $ 10 * 1000000
      return $ RequestTimeout req)
    (readMVar mvar)
  atomically $ modifyTVar' respMap (M.delete nextID)
  case res of
    Right r -> return r
    Left e  -> throw e

runSurreal :: MonadIO m => ConnectionState -> Surreal a -> m a
runSurreal cs surr = liftIO $ runReaderT surr cs

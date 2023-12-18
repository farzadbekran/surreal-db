{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Database.Surreal.WS.RPC.Surreal where

import           ClassyPrelude        hiding ( error, id )
import           Control.Concurrent   ( ThreadId, forkIO, myThreadId,
                                        threadDelay )
import           Control.Exception    ( throw )
import           Data.Aeson           ( FromJSON, ToJSON, Value, decode, encode,
                                        object, (.=) )
import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict      as M
import           Network.Socket       ( withSocketsDo )
import qualified Network.WebSockets   as WS

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
  | SigninError (Maybe Error)
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

data QueryResult
  = QueryResult
      { result :: Maybe Value
      , status :: Text
      , time   :: Text
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data Error
  = Error
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

{- |
listens to the connection and writes responses to the
response map.
-}
app :: MVar WS.Connection -> MVar ThreadId -> TVar (Map Int (MVar Response)) -> WS.ClientApp ()
app connMvar threadIDMvar respMap connection = do
  putMVar connMvar connection
  tid <- myThreadId
  putMVar threadIDMvar tid
  forever $ do
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
  threadIDMVar <- newEmptyMVar
  respTVar <- newTVarIO M.empty
  _ <- liftIO $ forkIO $ withSocketsDo $ WS.runClient url port "/rpc" (app connMVar threadIDMVar respTVar)
  conn <- readMVar connMVar
  tid <- readMVar threadIDMVar
  reqIDTvar <- newTVarIO 0
  let connectionState = ConnectionState conn reqIDTvar respTVar tid
  signinRes <- runSurreal connectionState $ send "signin"
    [object [ "user" .= user
            , "pass" .= pass
            , "ns" .= ns
            , "db" .= db]]
  case signinRes of
    Right Response { error } ->
      if isNothing error
        then return connectionState
        else throw $ SigninError error
    Left e -> throw e

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

runSurreal :: (MonadIO m) => ConnectionState -> Surreal a -> m (Either SomeException a)
runSurreal cs surr = liftIO $ try $ runReaderT surr cs

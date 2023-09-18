{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Database.Surreal
    ( ConnectionInfo (..)
    , QueryResult (..)
    , Response (..)
    , app
    , connect
    , defaultConnectionInfo
    , send
    ) where

import           ClassyPrelude        hiding ( error, id )
import           Control.Concurrent   ( forkIO, threadDelay )
import           Control.Exception    ( throw )
import           Data.Aeson           ( FromJSON, ToJSON, Value, decode,
                                        encode )
import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict      as M
import           GHC.IO               ( unsafePerformIO )
import           Network.Socket       ( withSocketsDo )
import qualified Network.WebSockets   as WS

conn :: TVar (Either SomeException WS.Connection)
{-# NOINLINE conn #-}
conn = unsafePerformIO $ newTVarIO $ Left $ SomeException NotConnected

nextReqID :: TVar Int
{-# NOINLINE nextReqID #-}
nextReqID = unsafePerformIO $ newTVarIO 0

-- A map of request id -> MVar Response.
-- Used to return the server response to the caller
-- `app` thread parses the server messages and writes them
-- to this map using the id included in the messages
respMap :: TVar (Map Int (MVar Response))
{-# NOINLINE respMap #-}
respMap = unsafePerformIO $ newTVarIO M.empty

data NetworkException
  = NotConnected
  | InvalidResponse Text
  | MissingMVar Int
  | RequestTimeout Request
  deriving (Exception, Show)

data Request
  = Request
      { id     :: Int
      , method :: Text
      , params :: [Text]
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data Response
  = Response
      { id     :: Int
      , result :: Maybe [QueryResult]
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
  deriving (Eq, Generic, FromJSON, Read, Show, ToJSON)

data ConnectionInfo
  = ConnectionInfo
      { url  :: String
      , port :: Int
      }
  deriving (Eq, Read, Show)

defaultConnectionInfo :: ConnectionInfo
defaultConnectionInfo = ConnectionInfo "0.0.0.0" 8000

app :: MVar SomeException -> WS.ClientApp ()
app exceptionMVar connection = do
  atomically $ writeTVar conn $ Right connection
  -- if anything throws, we write the exception into the exceptionMVar so that
  -- the thread started in the `connect` function can try to reconnect
  catchAny
    (forever $ do
        msg <- WS.receiveData connection
        let rsp = decode msg :: Maybe Response
        case rsp of
          Just r@Response { .. } -> do
            mvarMap <- readTVarIO respMap
            case mvarMap !? id of
              Just mvar -> putMVar mvar r
              Nothing   -> throw $ MissingMVar id
          Nothing -> throw $ InvalidResponse $ decodeUtf8 $ BL.toStrict msg)
    (\e -> do
        _ <- tryAny $ WS.sendClose connection ("Bye!" :: Text)
        atomically $ writeTVar conn $ Left e
        putMVar exceptionMVar e)

connect :: MonadIO m => ConnectionInfo -> m ()
connect ci@ConnectionInfo { .. } = do
  mCurrentConn <- readTVarIO conn
  -- we use this to reconnect to db in case client app throws
  exceptionMVar <- newEmptyMVar
  let doConnect = withSocketsDo $ catchAny
                   (WS.runClient url port "/rpc" (app exceptionMVar))
                   (putMVar exceptionMVar)
  _ <- liftIO $ case mCurrentConn of
    Left _ -> forkIO doConnect
    Right connection -> do
      -- try to close the connection if possible
      _ <- tryAny $ WS.sendClose connection ("Bye!" :: Text)
      forkIO $ withSocketsDo doConnect
  _ <- liftIO $ forkIO $ do
    res <- readMVar exceptionMVar
    atomically $ writeTVar conn $ Left res
    threadDelay $ 10 * 1000000
    connect ci
  return ()

getNextRequestID :: MonadIO m => m Int
getNextRequestID = atomically $ do
  nextID <- readTVar nextReqID
  writeTVar nextReqID $ nextID + 1
  return nextID

send :: ( MonadUnliftIO m) => Text -> [Text] -> m Response
send method params = do
  mCurrentConn <- readTVarIO conn
  case mCurrentConn of
    Left e -> throw e
    Right connection -> do
      nextID <- getNextRequestID
      mvar <- newEmptyMVar
      atomically $ modifyTVar' respMap (M.insert nextID mvar)
      let req = Request nextID method params
      liftIO $ WS.sendTextData connection $ encode req
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

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Surreal.WS.RPC where

import           ClassyPrelude                hiding ( error, id, throwTo )
import           Control.Concurrent           ( ThreadId, forkIO, myThreadId,
                                                threadDelay, throwTo )
import           Control.Exception            ( throw )
import           Control.Monad.Fail
import           Control.Monad.Trans          ( MonadTrans )
import           Data.Aeson                   ( Value (..), decode, encode,
                                                object, (.=) )
import qualified Data.Aeson.KeyMap            as AKM
import qualified Data.ByteString.Lazy         as BL
import           Data.Map.Strict              as M
import           Database.Surreal.Types
import           Database.Surreal.WS.RPCTypes
import           Network.Socket               ( withSocketsDo )
import qualified Network.WebSockets           as WS

newtype SurrealRPC a
  = SurrealRPC { unSurrealRPC :: ReaderT RPCConnectionState IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadIO
    , MonadReader RPCConnectionState
    , MonadUnliftIO
    )

instance MonadSurreal SurrealRPC where
  getNextRequestID = do
    cs <- ask
    runReaderT getNextRequestIDRPC cs
  send t vs = do
    cs <- ask
    runReaderT (sendRPC t vs) cs
  runQuery = runQueryRPC

runSurrealRPC :: RPCConnectionState -> SurrealRPC a -> IO a
runSurrealRPC cs m = runReaderT (unSurrealRPC m) cs

newtype SurrealRPCT m a
  = SurrealRPCT { unSurrealRPCT :: ReaderT RPCConnectionState m a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadIO
    , MonadReader RPCConnectionState
    , MonadUnliftIO
    )

instance (MonadIO m, MonadFail m) => MonadSurreal (SurrealRPCT m) where
  getNextRequestID = do
    cs <- ask
    liftIO $ runReaderT getNextRequestIDRPC cs
  send t vs = do
    cs <- ask
    liftIO $ runReaderT (sendRPC t vs) cs
  runQuery = runQueryRPC

instance (MonadIO m, MonadTrans t, MonadFail (t (SurrealRPCT m)), MonadFail m) => MonadSurreal (t (SurrealRPCT m)) where
  getNextRequestID = lift getNextRequestID
  send t vs = lift $ send t vs
  runQuery input query = lift $ runQuery input query

runSurrealRPCT :: RPCConnectionState -> SurrealRPCT m a -> m a
runSurrealRPCT cs action = runReaderT (unSurrealRPCT action) cs

{- |
listens to the connection and writes responses to the
response map.
-}
rpcApp :: MVar WS.Connection -> MVar ThreadId -> TVar (Map Int (MVar Response)) -> TVar (Map Text (LiveResponse -> IO ())) -> WS.ClientApp ()
rpcApp connMvar threadIDMvar respMap liveRespMap connection = do
  putMVar connMvar connection
  tid <- myThreadId
  putMVar threadIDMvar tid
  forever $ do
    msg <- WS.receiveData connection
    let rsp = decode msg :: Maybe Response
        liveNotification = decode msg :: Maybe LiveNotification
    case rsp of
      Just r@Response { .. } -> do
        mvarMap <- readTVarIO respMap
        case mvarMap !? id of
          Just mvar -> putMVar mvar r
          Nothing   -> putStrLn "rpcApp: Missing response MVar!"
      _ -> pure ()
    case liveNotification of
      Just (LiveNotification lr@(LiveResponse { .. })) -> do
        mvarMap <- readTVarIO liveRespMap
        case mvarMap !? id of
          Just f  -> void $ forkIO $ f lr
          Nothing -> putStrLn "rpcApp: Missing live response handler!"
      _ -> pure ()
    case (rsp, liveNotification) of
      (Nothing, Nothing) -> putStrLn $ "rpcApp: Invalid Response: " <> decodeUtf8 (BL.toStrict msg)
      _ -> pure ()

connectRPC :: MonadUnliftIO m => ConnectionInfo -> m RPCConnectionState
connectRPC ConnectionInfo { .. } = do
  connMVar <- newEmptyMVar
  threadIDMVar <- newEmptyMVar
  respTVar <- newTVarIO M.empty
  liveRespTVar <- newTVarIO M.empty
  parentThreadID <- liftIO myThreadId
  _ <- liftIO $ forkIO
    $ catchAny
    (withSocketsDo $ WS.runClient url port "/rpc" (rpcApp connMVar threadIDMVar respTVar liveRespTVar))
    (throwTo parentThreadID)
  conn <- readMVar connMVar
  tid <- readMVar threadIDMVar
  reqIDTvar <- newTVarIO 0
  let connectionState = RPCConnectionState conn reqIDTvar respTVar liveRespTVar tid
  signinRes <- runSignIn connectionState $ sendRPC "signin"
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
  where
    runSignIn :: MonadUnliftIO m => RPCConnectionState -> ReaderT RPCConnectionState m Response -> m (Either SomeException Response)
    runSignIn cs x = try $ runReaderT x cs

getNextRequestIDRPC :: (MonadIO m, MonadReader RPCConnectionState m) => m Int
getNextRequestIDRPC = do
  RPCConnectionState { .. } <- ask
  atomically $ do
    nextID <- readTVar nextReqID
    writeTVar nextReqID $ nextID + 1
    return nextID

sendRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => Text -> [Value] -> m Response
sendRPC method params = do
  RPCConnectionState { .. } <- ask
  nextID <- getNextRequestIDRPC
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

runQueryRPC :: MonadSurreal m => input -> Query input (Either DecodeError output) -> m output
runQueryRPC input (Query q encoder decoder) = do
  let encodedInput = encoder input
  Response { result = result, error = err } <- send "query" [String q, encodedInput]
  case err of
    Just e  -> throw e
    Nothing -> case result of
      Just (Array arr) -> do
        results <- mapM checkForErrorsAndExtractResults arr
        case decoder $ fromMaybe Null (lastMay results) of
          Right r -> return r
          Left e  -> fail $ show e
      v -> throw $ DecodeError
        $ "runQuery: Unexpected result format: expecting array but got: "
        <> pack (show v)
  where
    checkForErrorsAndExtractResults :: MonadSurreal m => Value -> m Value
    checkForErrorsAndExtractResults (Object o) = case o AKM.!? "status" of
      Just (String "OK") -> case o AKM.!? "result" of
        Just r -> return r
        Nothing -> throw $ DecodeError
          $ "runQuery: missing 'result' key in result map: " <> show o
      s -> throw $ DecodeError
        $ "runQuery: Unexpected status: "
        <> case s of
             Just (String err) -> unpack err
             a                 -> show $ fromMaybe "<NO STATUS MESSAGE>" a
        <> ". "
        <> case o AKM.!? "result" of
             Just (String msg) -> unpack msg
             a                 -> show $ fromMaybe "<NO ERROR MESSAGE>" a
    checkForErrorsAndExtractResults v = throw $ DecodeError
      $ "runQuery: Unexpected result format: expecting object but got: "
      <> pack (show v)

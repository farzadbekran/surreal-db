{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Database.Surreal.WS.RPC.Surreal where

import           ClassyPrelude          hiding ( error, id )
import           Control.Concurrent     ( ThreadId, forkIO, myThreadId,
                                          threadDelay )
import           Control.Exception      ( throw )
import           Data.Aeson             ( Value (..), decode, encode, object,
                                          (.=) )
import qualified Data.Aeson.KeyMap      as AKM
import qualified Data.ByteString.Lazy   as BL
import           Data.Map.Strict        as M
import           Database.Surreal.Types
import           Network.Socket         ( withSocketsDo )
import qualified Network.WebSockets     as WS

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

connectIO :: ConnectionInfo -> IO ConnectionState
connectIO ConnectionInfo { .. } = do
  connMVar <- newEmptyMVar
  threadIDMVar <- newEmptyMVar
  respTVar <- newTVarIO M.empty
  _ <- liftIO $ forkIO $ withSocketsDo $ WS.runClient url port "/rpc" (app connMVar threadIDMVar respTVar)
  conn <- readMVar connMVar
  tid <- readMVar threadIDMVar
  reqIDTvar <- newTVarIO 0
  let connectionState = ConnectionState conn reqIDTvar respTVar tid
  signinRes <- runSignIn connectionState $ sendIO "signin"
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
    runSignIn :: ConnectionState -> ReaderT ConnectionState IO Response -> IO (Either SomeException Response)
    runSignIn cs x = try $ runReaderT x cs

getNextRequestIDIO :: ReaderT ConnectionState IO Int
getNextRequestIDIO = do
  ConnectionState { .. } <- ask
  atomically $ do
    nextID <- readTVar nextReqID
    writeTVar nextReqID $ nextID + 1
    return nextID

sendIO :: Text -> [Value] -> ReaderT ConnectionState IO Response
sendIO method params = do
  ConnectionState { .. } <- ask
  nextID <- getNextRequestIDIO
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

runQueryIO :: MonadSurreal m => input -> Query input (m output) -> m output
runQueryIO input (Query q encoder decoder) = do
  let encodedInput = encoder input
  Response { result = result, error = err } <- send "query" [String q, encodedInput]
  case err of
    Just e  -> throw e
    Nothing -> case result of
      Just (Array arr) -> do
        results <- mapM checkForErrorsAndExtractResults arr
        decoder $ fromMaybe Null (lastMay results)
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
        $ "runQuery: Unexpected status: " <> show s
    checkForErrorsAndExtractResults v = throw $ DecodeError
      $ "runQuery: Unexpected result format: expecting object but got: "
      <> pack (show v)

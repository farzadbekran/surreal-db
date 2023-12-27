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

import           ClassyPrelude                 hiding ( error, id, throwTo )
import           Control.Concurrent            ( ThreadId, forkIO, myThreadId,
                                                 threadDelay, throwTo )
import           Control.Exception             ( throw )
import           Control.Monad.Catch           hiding ( try )
import           Control.Monad.Trans           ( MonadTrans )
import qualified Data.Aeson                    as J ( Value (..), decode,
                                                      encode, object, (.=) )
import qualified Data.Aeson.KeyMap             as AKM
import qualified Data.ByteString.Lazy          as BL
import           Data.Map.Strict               as M
import qualified Data.Vector                   as V
import           Database.Surreal.AST          ( Database, Identifier,
                                                 Namespace, ScopeName,
                                                 TableName, ToQL (..),
                                                 TokenValue, UserName )
import           Database.Surreal.MonadSurreal
import           Database.Surreal.TH
import           Database.Surreal.Types
import           Database.Surreal.WS.RPCTypes
import           Network.Socket                ( withSocketsDo )
import qualified Network.WebSockets            as WS

newtype SurrealRPC a
  = SurrealRPC { unSurrealRPC :: ReaderT RPCConnectionState IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader RPCConnectionState
    , MonadThrow
    , MonadUnliftIO
    )

instance MonadSurreal SurrealRPC where
  getNextRequestID = do
    cs <- ask
    runReaderT getNextRequestIDRPC cs
  send t vs = do
    cs <- ask
    runReaderT (sendRPC t vs) cs
  query = queryRPC
  registerLiveListener = registerLiveListenerRPC
  unregisterLiveListener = unregisterLiveListenerRPC
  use = useRPC
  info = infoRPC
  signup = signupRPC
  signin = signinRPC
  authenticate = authenticateRPC
  invalidate = invalidateRPC
  let_ = letRPC
  unset = unsetRPC
  live = liveRPC
  kill = killRPC
  select = selectRPC
  create = createRPC
  insert = insertRPC
  update = updateRPC
  merge = mergeRPC
  patch = patchRPC
  delete = deleteRPC

runSurrealRPC :: RPCConnectionState -> SurrealRPC a -> IO a
runSurrealRPC cs m = runReaderT (unSurrealRPC m) cs

newtype SurrealRPCT m a
  = SurrealRPCT { unSurrealRPCT :: ReaderT RPCConnectionState m a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader RPCConnectionState
    , MonadThrow
    , MonadUnliftIO
    )

instance (MonadThrow m, MonadUnliftIO m, MonadThrow (SurrealRPCT m)) => MonadSurreal (SurrealRPCT m) where
  getNextRequestID = do
    cs <- ask
    liftIO $ runReaderT getNextRequestIDRPC cs
  send t vs = do
    cs <- ask
    liftIO $ runReaderT (sendRPC t vs) cs
  query = queryRPC
  registerLiveListener = registerLiveListenerRPC
  unregisterLiveListener = unregisterLiveListenerRPC
  use = useRPC
  info = infoRPC
  signup = signupRPC
  signin = signinRPC
  authenticate = authenticateRPC
  invalidate = invalidateRPC
  let_ = letRPC
  unset = unsetRPC
  live = liveRPC
  kill = killRPC
  select = selectRPC
  create = createRPC
  insert = insertRPC
  update = updateRPC
  merge = mergeRPC
  patch = patchRPC
  delete = deleteRPC

instance ( MonadUnliftIO m
         , MonadTrans t
         , MonadThrow (t (SurrealRPCT m))
         , MonadThrow (SurrealRPCT m)
         , MonadThrow m) =>
         MonadSurreal (t (SurrealRPCT m)) where
  getNextRequestID = lift getNextRequestID
  send t vs = lift $ send t vs
  query input q = lift $ query input q
  registerLiveListener t handler = lift $ registerLiveListenerRPC t handler
  unregisterLiveListener t = lift $ unregisterLiveListenerRPC t
  use ns db = lift $ useRPC ns db
  info = lift infoRPC
  signup ns db scope km = lift $ signupRPC ns db scope km
  signin us pass ns db scope km = lift $ signinRPC us pass ns db scope km
  authenticate token = lift $ authenticateRPC token
  invalidate = lift invalidateRPC
  let_ i v = lift $ letRPC i v
  unset i = lift $ unsetRPC i
  live tn = lift $ liveRPC tn
  kill i = lift $ killRPC i
  select t = lift $ selectRPC t
  create t mVal = lift $ createRPC t mVal
  insert tn vs = lift $ insertRPC tn vs
  update t mVal = lift $ updateRPC t mVal
  merge t mVal = lift $ mergeRPC t mVal
  patch t vs  = lift $ patchRPC t vs
  delete t = lift $ deleteRPC t

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
    let rsp = J.decode msg :: Maybe Response
        liveNotification = J.decode msg :: Maybe LiveNotification
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
               [J.object [ "user" J..= user
                         , "pass" J..= pass
                         , "ns" J..= ns
                         , "db" J..= db]]
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

sendRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => Text -> [J.Value] -> m Response
sendRPC method params = do
  RPCConnectionState { .. } <- ask
  nextID <- getNextRequestIDRPC
  mvar <- newEmptyMVar
  atomically $ modifyTVar' respMap (M.insert nextID mvar)
  let req = Request nextID method params
  liftIO $ WS.sendTextData conn $ J.encode req
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

queryRPC :: MonadSurreal m => input -> Query input (Either DecodeError output) -> m output
queryRPC input q = do
  let encodedInput = getEncoder q input
  Response { result = result, error = err } <- send "query" [J.String (getSQL q), encodedInput]
  case err of
    Just e  -> throw e
    Nothing -> case result of
      Just (J.Array arr) -> do
        results <- mapM checkForErrorsAndExtractResults arr
        case getDecoder q $ fromMaybe J.Null (lastMay results) of
          Right r -> return r
          Left e  -> throw e
      v -> throw $ DecodeError
        $ "runQuery: Unexpected result format: expecting array but got: "
        <> pack (show v)
  where
    checkForErrorsAndExtractResults :: MonadSurreal m => J.Value -> m J.Value
    checkForErrorsAndExtractResults (J.Object o) = case o AKM.!? "status" of
      Just (J.String "OK") -> case o AKM.!? "result" of
        Just r -> return r
        Nothing -> throw $ DecodeError
          $ "runQuery: missing 'result' key in result map: " <> show o
      s -> throw $ DecodeError
        $ "runQuery: Unexpected status: "
        <> case s of
             Just (J.String err) -> unpack err
             a                   -> show $ fromMaybe "<NO STATUS MESSAGE>" a
        <> ". "
        <> case o AKM.!? "result" of
             Just (J.String msg) -> unpack msg
             a                   -> show $ fromMaybe "<NO ERROR MESSAGE>" a
    checkForErrorsAndExtractResults v = throw $ DecodeError
      $ "runQuery: Unexpected result format: expecting object but got: "
      <> pack (show v)

registerLiveListenerRPC :: ( MonadIO m, MonadReader RPCConnectionState m) => Text -> (LiveResponse -> IO ()) -> m ()
registerLiveListenerRPC uuid handler = do
  RPCConnectionState { .. } <- ask
  atomically $ modifyTVar' liveRespMap (M.insert uuid handler)

unregisterLiveListenerRPC :: ( MonadReader RPCConnectionState m, MonadUnliftIO m) => Text -> m ()
unregisterLiveListenerRPC uuid = do
  Response { error = err } <- sendRPC "query" [J.String $ "KILL \"" <> uuid <> "\";"]
  forM_ err throw
  RPCConnectionState { .. } <- ask
  atomically $ modifyTVar' liveRespMap (M.delete uuid)

-- | Makes sure the response contains no error and returns the value from the `result` key
getResponseValue :: Response -> Either SurrealError (Maybe J.Value)
getResponseValue Response { result = result, error = err } =
  case err of
    Nothing -> Right result
    Just e  -> Left e

useRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => Namespace -> Database -> m ()
useRPC ns db = do
  r <- getResponseValue <$> sendRPC "use" [J.String $ toQL ns, J.String $ toQL db]
  case r of
    Right _ -> return ()
    Left e  -> throw e

infoRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => m (Maybe J.Value)
infoRPC = do
  r <- getResponseValue <$> sendRPC "info" []
  case r of
    Right r' -> return r'
    Left e   -> throw e

signupRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) =>
  Namespace -> Database -> ScopeName -> J.Value -> m (Maybe J.Value)
signupRPC ns db scope (J.Object km) = do
  let p = J.Object $ ( AKM.insert "NS" (J.String $ toQL ns)
                       . AKM.insert "DB" (J.String $ toQL db)
                       . AKM.insert "SC" (J.String $ toQL scope) )
          km
  r <- getResponseValue <$> sendRPC "signup" [p]
  case r of
    Right r' -> return r'
    Left e   -> throw e
signupRPC _ _ _ v = throw $ DriverError $ "signupRPC: Expected a JSON Object but got: " <> show v

signinRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) =>
  UserName -> Password -> Maybe Namespace -> Maybe Database -> Maybe ScopeName -> J.Value -> m TokenValue
signinRPC un pass ns db scope (J.Object km) = do
  let p = J.Object $ ( AKM.insert "user" (J.String $ toQL un)
                       . AKM.insert "pass" (J.String $ toQL pass)
                       . AKM.insert "NS" (J.String $ toQL ns)
                       . AKM.insert "DB" (J.String $ toQL db)
                       . AKM.insert "SC" (J.String $ toQL scope) )
          km
  r <- getResponseValue <$> sendRPC "signin" [p]
  case r of
    Right (Just (J.String r')) -> return r'
    Left e   -> throw e
    Right v -> throw $ DriverError $ "signinRPC: Unexpected response: " <> show v
signinRPC _ _ _ _ _ v = throw $ DriverError $ "signinRPC: Expected a JSON Object but got: " <> show v

authenticateRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => TokenValue -> m ()
authenticateRPC token = do
  r <- getResponseValue <$> sendRPC "authenticate" [J.String token]
  case r of
    Right _ -> return ()
    Left e  -> throw e

invalidateRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => m ()
invalidateRPC = do
  r <- getResponseValue <$> sendRPC "invalidate" []
  case r of
    Right _ -> return ()
    Left e  -> throw e

letRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => Identifier -> J.Value -> m ()
letRPC i v = do
  r <- getResponseValue <$> sendRPC "let" [J.String $ toQL i, v]
  case r of
    Right _ -> return ()
    Left e  -> throw e

unsetRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => Identifier -> m ()
unsetRPC i = do
  r <- getResponseValue <$> sendRPC "unset" [J.String $ toQL i]
  case r of
    Right _ -> return ()
    Left e  -> throw e

liveRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => TableName -> m LiveQueryUUID
liveRPC tn = do
  r <- getResponseValue <$> sendRPC "live" [J.String $ toQL tn]
  case r of
    Right (Just (J.String r')) -> return r'
    Left e   -> throw e
    Right v -> throw $ DriverError $ "signinRPC: Unexpected response: " <> show v

killRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => LiveQueryUUID -> m ()
killRPC i = do
  r <- getResponseValue <$> sendRPC "kill" [J.String $ toQL i]
  case r of
    Right _ -> return ()
    Left e  -> throw e

selectRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> m (Maybe J.Value)
selectRPC target = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "select" [J.String t]
  case r of
    Right r' -> return r'
    Left e   -> throw e

createRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> Maybe J.Value -> m J.Value
createRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "create" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throw $ DriverError $ "createRPC: Unexpected result: " <> show x
    Left e   -> throw e

insertRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => TableName -> [J.Value] -> m [J.Value]
insertRPC tn vs = do
  r <- getResponseValue <$> sendRPC "create" (J.String (toQL tn) : vs)
  case r of
    Right (Just (J.Array r')) -> return $ V.toList r'
    Right x -> throw $ DriverError $ "insertRPC: Unexpected result: " <> show x
    Left e   -> throw e

updateRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> Maybe J.Value -> m J.Value
updateRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "update" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throw $ DriverError $ "updateRPC: Unexpected result: " <> show x
    Left e   -> throw e

mergeRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> Maybe J.Value -> m J.Value
mergeRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "merge" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throw $ DriverError $ "mergeRPC: Unexpected result: " <> show x
    Left e   -> throw e

patchRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> [J.Value] -> m J.Value
patchRPC target vs = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "patch" (J.String t : vs)
  case r of
    Right (Just r') -> return r'
    Right x -> throw $ DriverError $ "patchRPC: Unexpected result: " <> show x
    Left e   -> throw e

deleteRPC :: (MonadUnliftIO m, MonadReader RPCConnectionState m) => OPTarget -> m J.Value
deleteRPC target = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "delete" [J.String t]
  case r of
    Right (Just r') -> return r'
    Right x -> throw $ DriverError $ "patchRPC: Unexpected result: " <> show x
    Left e   -> throw e

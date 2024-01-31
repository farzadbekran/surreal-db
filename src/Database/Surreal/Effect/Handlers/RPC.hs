{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE IncoherentInstances        #-}

module Database.Surreal.Effect.Handlers.RPC where

import           ClassyPrelude                             hiding (race, Reader, ask,
                                                             error, id,
                                                             throwTo )
import qualified Control.Concurrent                        as C
--import           Control.Exception                         ( throw )
import qualified Data.Aeson                                as J ( Value (..),
                                                                  decode,
                                                                  encode,
                                                                  object, (.=) )
import qualified Data.Aeson.KeyMap                         as AKM
import qualified Data.ByteString.Lazy                      as BL
import           Data.Map.Strict                           as M
import qualified Data.Vector                               as V
import           Database.Surreal.AST                      ( Database,
                                                             Identifier,
                                                             Namespace,
                                                             ScopeName,
                                                             TableName,
                                                             ToQL (..),
                                                             TokenValue,
                                                             UserName )
import           Database.Surreal.Effect
import           Database.Surreal.Effect.Handlers.RPCTypes
import           Database.Surreal.TH
import           Database.Surreal.Types
import           Effectful
import           Effectful.Concurrent
import           Effectful.Concurrent.Async
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Dynamic
import           Effectful.Reader.Dynamic
import           Network.Socket                            ( withSocketsDo )
import qualified Network.WebSockets                        as WS

type RPCConstraints es = (IOE :> es, Reader RPCConnectionState :> es, Error RPCError :> es, Concurrent :> es)

{- |
listens to the connection and writes responses to the
response map.
-}
rpcApp
  :: MVar WS.Connection
  -> MVar C.ThreadId
  -> TVar (Map Int (MVar Response))
  -> TVar (Map Text (LiveResponse -> IO ()))
  -> WS.ClientApp ()
rpcApp connMvar threadIDMvar respMap liveRespMap connection = do
  putMVar connMvar connection
  tid <- C.myThreadId
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
      _otherwise -> pure ()
    case liveNotification of
      Just (LiveNotification lr@(LiveResponse { .. })) -> do
        mvarMap <- readTVarIO liveRespMap
        case mvarMap !? id of
          Just f  -> void $ C.forkIO $ f lr
          Nothing -> putStrLn "rpcApp: Missing live response handler!"
      _otherwise -> pure ()
    case (rsp, liveNotification) of
      (Nothing, Nothing) -> putStrLn $ "rpcApp: Invalid Response: " <> decodeUtf8 (BL.toStrict msg)
      _otherwise -> pure ()

connectRPC :: (IOE :> es, Error RPCError :> es, Concurrent :> es) => ConnectionInfo -> Eff es RPCConnectionState
connectRPC ConnectionInfo { .. } = catchAny
  (do
    connMVar <- newEmptyMVar
    threadIDMVar <- newEmptyMVar
    respTVar <- newTVarIO M.empty
    liveRespTVar <- newTVarIO M.empty
    parentThreadID <- myThreadId
    _ <- forkIO
      $ catchAny
      (liftIO $ withSocketsDo $ WS.runClient url port "/rpc" (rpcApp connMVar threadIDMVar respTVar liveRespTVar))
      (throwTo parentThreadID)
    conn <- readMVar connMVar
    tid <- readMVar threadIDMVar
    reqIDTvar <- newTVarIO 0
    let connectionState = RPCConnectionState conn reqIDTvar respTVar liveRespTVar tid
    _ <- runReader connectionState $ sendRPC "signin"
                 [J.object [ "user" J..= user
                           , "pass" J..= pass
                           , "ns" J..= ns
                           , "db" J..= db]]
    return connectionState
    --case signinRes of
    --  Right Response { error } ->
    --    if isNothing error
    --      then return connectionState
    --      else throw $ SigninError error
    --  Left (_cs, e) -> throwError e
    )
  (throwError . IOException)

getNextRequestIDRPC :: RPCConstraints es => Eff es Int
getNextRequestIDRPC = do
  RPCConnectionState { .. } <- ask
  atomically $ do
    nextID <- readTVar nextReqID
    writeTVar nextReqID $ nextID + 1
    return nextID

sendRPC :: RPCConstraints es => Text -> [J.Value] -> Eff es Response
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
      threadDelay $ 10 * 1000000
      return $ RequestTimeout req)
    (readMVar mvar)
  atomically $ modifyTVar' respMap (M.delete nextID)
  case res of
    Right r -> return r
    Left e  -> throwError e

queryRPC :: RPCConstraints es => input -> Query input (Either DecodeError output) -> Eff es output
queryRPC input q = do
  let encodedInput = getEncoder q input
  Response { result = result, error = err } <- sendRPC "query" [J.String (getSQL q), encodedInput]
  case err of
    Just e  -> throwError $ SurrealErr e
    Nothing -> case result of
      Just (J.Array arr) -> do
        results <- mapM checkForErrorsAndExtractResults arr
        case getDecoder q $ fromMaybe J.Null (lastMay results) of
          Right r -> return r
          Left e  -> throwError $ DecodeErr e
      v -> throwError $ DecodeErr $ DecodeError
        $ "runQuery: Unexpected result format: expecting array but got: "
        <> pack (show v)
  where
    checkForErrorsAndExtractResults :: RPCConstraints es => J.Value -> Eff es J.Value
    checkForErrorsAndExtractResults (J.Object o) = case o AKM.!? "status" of
      Just (J.String "OK") -> case o AKM.!? "result" of
        Just r -> return r
        Nothing -> throwError $ DecodeErr $ DecodeError
          $ "runQuery: missing 'result' key in result map: " <> show o
      s -> throwError $ DecodeErr $ DecodeError
        $ "runQuery: Unexpected status: "
        <> case s of
             Just (J.String err) -> unpack err
             a                   -> show $ fromMaybe "<NO STATUS MESSAGE>" a
        <> ". "
        <> case o AKM.!? "result" of
             Just (J.String msg) -> unpack msg
             a                   -> show $ fromMaybe "<NO ERROR MESSAGE>" a
    checkForErrorsAndExtractResults v = throwError $ DecodeErr $ DecodeError
      $ "runQuery: Unexpected result format: expecting object but got: "
      <> pack (show v)

registerLiveListenerRPC :: RPCConstraints es => Text -> (LiveResponse -> IO ()) -> Eff es ()
registerLiveListenerRPC uuid handler = do
  RPCConnectionState { .. } <- ask
  atomically $ modifyTVar' liveRespMap (M.insert uuid handler)

unregisterLiveListenerRPC :: RPCConstraints es => Text -> Eff es ()
unregisterLiveListenerRPC uuid = do
  Response { error = err } <- sendRPC "query" [J.String $ "KILL \"" <> uuid <> "\";"]
  forM_ err (throwError . SurrealErr)
  RPCConnectionState { .. } <- ask
  atomically $ modifyTVar' liveRespMap (M.delete uuid)

-- | Makes sure the response contains no error and returns the value from the `result` key
getResponseValue :: Response -> Either SurrealError (Maybe J.Value)
getResponseValue Response { result = result, error = err } =
  case err of
    Nothing -> Right result
    Just e  -> Left e

useRPC :: RPCConstraints es => Namespace -> Database -> Eff es ()
useRPC ns db = do
  r <- getResponseValue <$> sendRPC "use" [J.String $ toQL ns, J.String $ toQL db]
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

infoRPC :: RPCConstraints es => Eff es (Maybe J.Value)
infoRPC = do
  r <- getResponseValue <$> sendRPC "info" []
  case r of
    Right r' -> return r'
    Left e   -> throwError $ SurrealErr e

signupRPC :: RPCConstraints es =>
  Namespace -> Database -> ScopeName -> J.Value -> Eff es (Maybe J.Value)
signupRPC ns db scope (J.Object km) = do
  let p = J.Object $ ( AKM.insert "NS" (J.String $ toQL ns)
                       . AKM.insert "DB" (J.String $ toQL db)
                       . AKM.insert "SC" (J.String $ toQL scope) )
          km
  r <- getResponseValue <$> sendRPC "signup" [p]
  case r of
    Right r' -> return r'
    Left e   -> throwError $ SurrealErr e
signupRPC _ _ _ v = throwError $ DriverErr
  $ DriverError $ "signupRPC: Expected a JSON Object but got: " <> show v

signinRPC :: RPCConstraints es =>
  UserName -> Password -> Maybe Namespace -> Maybe Database -> Maybe ScopeName -> J.Value -> Eff es TokenValue
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
    Left e   -> throwError $ SurrealErr e
    Right v -> throwError $ DriverErr
      $ DriverError $ "signinRPC: Unexpected response: " <> show v
signinRPC _ _ _ _ _ v = throwError $ DriverErr
  $ DriverError $ "signinRPC: Expected a JSON Object but got: " <> show v

authenticateRPC :: RPCConstraints es => TokenValue -> Eff es ()
authenticateRPC token = do
  r <- getResponseValue <$> sendRPC "authenticate" [J.String token]
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

invalidateRPC :: RPCConstraints es => Eff es ()
invalidateRPC = do
  r <- getResponseValue <$> sendRPC "invalidate" []
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

letRPC :: RPCConstraints es => Identifier -> J.Value -> Eff es ()
letRPC i v = do
  r <- getResponseValue <$> sendRPC "let" [J.String $ toQL i, v]
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

unsetRPC :: RPCConstraints es => Identifier -> Eff es ()
unsetRPC i = do
  r <- getResponseValue <$> sendRPC "unset" [J.String $ toQL i]
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

liveRPC :: RPCConstraints es => TableName -> Eff es LiveQueryUUID
liveRPC tn = do
  r <- getResponseValue <$> sendRPC "live" [J.String $ toQL tn]
  case r of
    Right (Just (J.String r')) -> return r'
    Left e   -> throwError $ SurrealErr e
    Right v -> throwError $ DriverErr $ DriverError $ "signinRPC: Unexpected response: " <> show v

killRPC :: RPCConstraints es => LiveQueryUUID -> Eff es ()
killRPC i = do
  r <- getResponseValue <$> sendRPC "kill" [J.String $ toQL i]
  case r of
    Right _ -> return ()
    Left e  -> throwError $ SurrealErr e

selectRPC :: RPCConstraints es => OPTarget -> Eff es (Maybe J.Value)
selectRPC target = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "select" [J.String t]
  case r of
    Right r' -> return r'
    Left e   -> throwError $ SurrealErr e

createRPC :: RPCConstraints es => OPTarget -> Maybe J.Value -> Eff es J.Value
createRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "create" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throwError $ DriverErr $ DriverError $ "createRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

insertRPC :: RPCConstraints es => TableName -> [J.Value] -> Eff es [J.Value]
insertRPC tn vs = do
  r <- getResponseValue <$> sendRPC "create" (J.String (toQL tn) : vs)
  case r of
    Right (Just (J.Array r')) -> return $ V.toList r'
    Right x -> throwError $ DriverErr $ DriverError $ "insertRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

updateRPC :: RPCConstraints es => OPTarget -> Maybe J.Value -> Eff es J.Value
updateRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "update" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throwError $ DriverErr $ DriverError $ "updateRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

mergeRPC :: RPCConstraints es => OPTarget -> Maybe J.Value -> Eff es J.Value
mergeRPC target mVal = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "merge" (J.String t : case mVal of
                                                 Just v -> [v]
                                                 _      -> [])
  case r of
    Right (Just r') -> return r'
    Right x -> throwError $ DriverErr $ DriverError $ "mergeRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

patchRPC :: RPCConstraints es => OPTarget -> [J.Value] -> Eff es J.Value
patchRPC target vs = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "patch" (J.String t : vs)
  case r of
    Right (Just r') -> return r'
    Right x -> throwError $ DriverErr $ DriverError $ "patchRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

deleteRPC :: RPCConstraints es => OPTarget -> Eff es J.Value
deleteRPC target = do
  let t = case target of
            Table tn   -> toQL tn
            Record rid -> toQL rid
  r <- getResponseValue <$> sendRPC "delete" [J.String t]
  case r of
    Right (Just r') -> return r'
    Right x -> throwError $ DriverErr $ DriverError $ "patchRPC: Unexpected result: " <> show x
    Left e   -> throwError $ SurrealErr e

runSurrealRPC :: (IOE :> es, Error RPCError :> es) => RPCConnectionState -> Eff (Surreal : es) a -> Eff es a
runSurrealRPC conn = reinterpret (runConcurrent . runReader conn) $ \_ -> \case
  GetNextRequestID -> getNextRequestIDRPC
  Send_ s vals -> sendRPC s vals
  Query i q -> queryRPC i q
  RegisterLiveListener uuid listener -> registerLiveListenerRPC uuid listener
  UnregisterLiveListener uuid -> unregisterLiveListenerRPC uuid
  Use ns db -> useRPC ns db
  Info -> infoRPC
  Signup ns db scope val -> signupRPC ns db scope val
  Signin un pass mNS mDB mSC val -> signinRPC un pass mNS mDB mSC val
  Authenticate token -> authenticateRPC token
  Invalidate -> invalidateRPC
  Let_ id v -> letRPC id v
  Unset id -> unsetRPC id
  Live tn -> liveRPC tn
  Kill uuid -> killRPC uuid
  Select t -> selectRPC t
  Create t mV -> createRPC t mV
  Insert tn vs -> insertRPC tn vs
  Update t mV -> updateRPC t mV
  Merge t mV -> mergeRPC t mV
  Patch t vs -> patchRPC t vs
  Delete t -> deleteRPC t

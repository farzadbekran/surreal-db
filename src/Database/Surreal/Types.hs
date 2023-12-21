{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Database.Surreal.Types where

import           ClassyPrelude        hiding ( error, id )
import           Control.Concurrent   hiding ( newEmptyMVar, putMVar, readMVar )
import           Control.Exception    ( throw )
import           Data.Aeson           as J
import           Data.Aeson.KeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as M
import           Network.Socket       ( withSocketsDo )
import qualified Network.WebSockets   as WS

class Monad m => MonadSurreal m where
  getNextRequestID :: m Int
  send :: Text -> [Value] -> m Response
  runQuery :: input -> Query input (m output) -> m output

-- instance (MonadUnliftIO m) => MonadSurreal (ReaderT ConnectionState m) where
--   send method params = do
--     ConnectionState { .. } <- ask
--     nextID <- getNextRequestID
--     mvar <- newEmptyMVar
--     atomically $ modifyTVar' respMap (M.insert nextID mvar)
--     let req = Request nextID method params
--     liftIO $ WS.sendTextData conn $ encode req
--     -- recover after 10 seconds of delay
--     res <- race
--       (do
--         liftIO $ threadDelay $ 10 * 1000000
--         return $ RequestTimeout req)
--       (readMVar mvar)
--     atomically $ modifyTVar' respMap (M.delete nextID)
--     case res of
--       Right r -> return r
--       Left e  -> throw e

--   runQuery input (Query q encoder decoder) = do
--     let encodedInput = encoder input
--     r@Response { result = result, error = err } <- send "query" [String q, encodedInput]
--     print r
--     case err of
--       Just e  -> throwIO e
--       Nothing -> case result of
--         Just (Array arr) -> do
--           results <- mapM checkForErrorsAndExtractResults arr
--           decoder $ fromMaybe Null (lastMay results)
--         v -> throwIO $ DecodeError
--           $ "runQuery: Unexpected result format: expecting array but got: "
--           <> pack (show v)
--     where
--       checkForErrorsAndExtractResults :: MonadIO m => Value -> ReaderT ConnectionState m Value
--       checkForErrorsAndExtractResults (Object o) = case o !? "status" of
--         Just (String "OK") -> case o !? "result" of
--           Just r -> return r
--           Nothing -> throwIO $ DecodeError
--             $ "runQuery: missing 'result' key in result map: " <> show o
--         s -> throwIO $ DecodeError
--           $ "runQuery: Unexpected status: " <> show s
--       checkForErrorsAndExtractResults v = throwIO $ DecodeError
--         $ "runQuery: Unexpected result format: expecting object but got: "
--         <> pack (show v)

--   getNextRequestID = do
--     ConnectionState { .. } <- ask
--     atomically $ do
--       nextID <- readTVar nextReqID
--       writeTVar nextReqID $ nextID + 1
--       return nextID

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
  | SigninError (Maybe SurrealError)
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
      , error  :: Maybe SurrealError
      }
  deriving (Eq, FromJSON, Generic, Read, Show, ToJSON)

data QueryResult
  = QueryResult
      { result :: Maybe Value
      , status :: Text
      , time   :: Text
      }
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

-- | The type used by TH to parse SurrealQL
data Query input output
  = Query Text (input -> Value) (Value -> output)

newtype DecodeError
  = DecodeError String
  deriving (Exception, Show)

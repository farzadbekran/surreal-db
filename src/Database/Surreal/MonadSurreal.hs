{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}

module Database.Surreal.MonadSurreal where

import           ClassyPrelude          hiding ( error, id )
import           Control.Monad.Catch
import           Data.Aeson             as J
import           Data.Aeson.KeyMap
import           Database.Surreal.AST   ( Database, Namespace, ScopeName,
                                          TokenValue, UserName, Identifier, TableName )
import           Database.Surreal.TH
import           Database.Surreal.Types

type Password = Text
type LiveQueryUUID = Text

class MonadThrow m => MonadSurreal m where
  -- | Used internally to generate incrementing ids for requests
  getNextRequestID :: m Int
  -- | Low level function used to send queries
  send :: Text -> [Value] -> m Response
  -- | High level function to run a query against database.
  --The `Query` is produced by `sql` TemplateHaskell quasi quoter
  --which makes the query type-safe and well supported
  runQuery :: input -> Query input (Either DecodeError output) -> m output
  -- | Registers a listener in the driver to handle `LiveResponse`s received
  --from server. You get `LiveQueryUUID` by running a 'live select ...' query
  --with `runQuery` function or running a `live` function
  registerLiveListener :: LiveQueryUUID -> (LiveResponse -> IO ()) -> m ()
  -- | Removes a listener from the driver and sends a `kill` command with
  --the given id to the server
  unregisterLiveListener :: LiveQueryUUID -> m ()
  -- | This method specifies the namespace and database for the current connection
  --unlike what is shown in the surreal docs, ns and db params are required!
  use :: Namespace -> Database -> m ()
  -- | This method returns the record of an authenticated scope user.
  info :: m (Maybe Value)
  -- | This method allows you to signup a user against a scope's SIGNUP method
  signup :: Namespace -> Database -> ScopeName -> KeyMap Value -> m (Maybe Value)
  -- | This method allows you to signin a root, NS, DB or SC user against SurrealDB
  signin :: UserName -> Password -> Maybe Namespace -> Maybe Database -> Maybe ScopeName -> KeyMap Value -> m TokenValue
  -- | Authenticate a user against SurrealDB with a token
  authenticate :: TokenValue -> m ()
  -- | This method will invalidate the user's session for the current connection
  invalidate :: m ()
  -- | Define a variable on the current connection
  let_ :: Identifier -> Value -> m ()
  -- | Remove a variable from the current connection
  unset :: Identifier -> m ()
  -- | Initiate a live query
  live :: TableName -> m LiveQueryUUID



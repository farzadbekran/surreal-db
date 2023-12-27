{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}

module Database.Surreal.MonadSurreal where

import           ClassyPrelude          hiding ( error, id )
import           Control.Monad.Catch
import           Data.Aeson             as J
import           Database.Surreal.AST   ( Database, Identifier, Namespace,
                                          RecordID, ScopeName, TableName,
                                          TokenValue, UserName )
import           Database.Surreal.TH
import           Database.Surreal.Types

type Password = Text
type LiveQueryUUID = Text
type SQL = Text

data OPTarget
  = Table TableName
  | Record RecordID
  deriving (Eq, Generic, Show)

class MonadThrow m => MonadSurreal m where
  -- | Used internally to generate incrementing ids for requests
  getNextRequestID :: m Int
  -- | Low level function used to send queries
  send :: SQL -> [Value] -> m Response
  -- | High level function to run a query against database.
  --The `Query` is produced by `sql` TemplateHaskell quasi quoter
  --which makes the query type-safe using type annotations
  query :: input -> Query input (Either DecodeError output) -> m output
  -- | Registers a listener in the driver to handle `LiveResponse`s received
  --from server. You get `LiveQueryUUID` by running a 'live select ...' query
  --with `runQuery` function or running a `live` function
  registerLiveListener :: LiveQueryUUID -> (LiveResponse -> IO ()) -> m ()
  -- | Removes a listener from the driver and sends a `kill` command with
  --the given id to the server
  unregisterLiveListener :: LiveQueryUUID -> m ()
  -- | This method specifies the namespace and database for the currenartist:00b2pg847d7b8r08t08tt connection
  --unlike what is shown in the surreal docs, ns and db params are required!
  use :: Namespace -> Database -> m ()
  -- | This method returns the record of an authenticated scope user.
  info :: m (Maybe Value)
  -- | This method allows you to signup a user against a scope's SIGNUP method
  --The value should be a JSON Object, containing extra params
  signup :: Namespace -> Database -> ScopeName -> Value -> m (Maybe Value)
  -- | This method allows you to signin a root, NS, DB or SC user against SurrealDB
  --The value should be a JSON Object, containing extra params
  signin :: UserName -> Password -> Maybe Namespace -> Maybe Database -> Maybe ScopeName -> Value -> m TokenValue
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
  -- | Kill an active live query
  kill :: LiveQueryUUID -> m ()
  -- | This method selects either all records in a table or a single record
  --using a record id
  select :: OPTarget -> m (Maybe Value)
  -- | This method creates a record either with a random or specified ID
  create :: OPTarget -> Maybe Value -> m Value
  -- | Insert one or multiple records in a table
  insert :: TableName -> [Value] -> m [Value]
  -- | Update a specified record or table with the given value,
  --if no value is given, simply trigger an update event on the target
  update :: OPTarget -> Maybe Value -> m Value
  -- | This method merges specified data into either all records in a table or a single record
  merge :: OPTarget -> Maybe Value -> m Value
  -- | Patch a record or table with the given JSON Patches
  patch :: OPTarget -> [Value] -> m Value
  -- | Delete a record or whole contents of a table
  delete :: OPTarget -> m Value

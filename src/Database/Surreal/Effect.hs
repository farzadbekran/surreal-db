{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Surreal.Effect where

import           ClassyPrelude                  hiding ( delete )

import           Data.Aeson                     as J
import           Database.Surreal.AST           ( Database, Identifier,
                                                  Namespace, RecordID,
                                                  ScopeName, TableName,
                                                  TokenValue, UserName )
import           Database.Surreal.Class.ToParam
import           Database.Surreal.TH
import           Database.Surreal.Types
import           Effectful
import           Effectful.TH

type Password = Text
type LiveQueryUUID = Text
type SQL = Text

data OPTarget
  = Table !TableName
  | Record !RecordID
  deriving (Eq, Generic, Show)

data Surreal :: Effect where
  -- | Used internally to generate incrementing ids for requests
  GetNextRequestID :: Surreal m Int
  -- | Low level function used to send queries
  Send_ :: SQL -> [Text] -> Surreal m Response
  -- | High level function to run a query against database.
  --The `Query` is produced by `sql` TemplateHaskell quasi quoter
  --which makes the query type-safe using type annotations
  Query :: input -> Query input (Either DecodeError output) -> Surreal m output
  -- | Registers a listener in the driver to handle `LiveResponse`s received
  --from server. You get `LiveQueryUUID` by running a 'live select ...' query
  --with `runQuery` function or running a `live` function
  RegisterLiveListener :: LiveQueryUUID -> (LiveResponse -> IO ()) -> Surreal m ()
  -- | Removes a listener from the driver and sends a `kill` command with
  --the given id to the server
  UnregisterLiveListener :: LiveQueryUUID -> Surreal m ()
  -- | This method specifies the namespace and database for the current connection
  --unlike what is shown in the surreal docs, ns and db params are required!
  Use :: Namespace -> Database -> Surreal m ()
  -- | This method returns the record of an authenticated scope user.
  Info :: Surreal m (Maybe Value)
  -- | This method allows you to signup a user against a scope's SIGNUP method
  --The value should be a JSON Object, containing extra params
  Signup :: Namespace -> Database -> ScopeName -> Value -> Surreal m (Maybe Value)
  -- | This method allows you to signin a root, NS, DB or SC user against SurrealDB
  --The value should be a JSON Object, containing extra params
  Signin :: UserName -> Password -> Maybe Namespace -> Maybe Database -> Maybe ScopeName -> Value -> Surreal m TokenValue
  -- | Authenticate a user against SurrealDB with a token
  Authenticate :: TokenValue -> Surreal m ()
  -- | This method will invalidate the user's session for the current connection
  Invalidate :: Surreal m ()
  -- | Define a variable on the current connection
  Let_ :: Identifier -> Value -> Surreal m ()
  -- | Remove a variable from the current connection
  Unset :: Identifier -> Surreal m ()
  -- | Initiate a live query
  Live :: TableName -> Surreal m LiveQueryUUID
  -- | Kill an active live query
  Kill :: LiveQueryUUID -> Surreal m ()
  -- | This method selects either all records in a table or a single record
  --using a record id
  Select :: OPTarget -> Surreal m (Maybe Value)
  -- | This method creates a record either with a random or specified ID
  Create :: ToParam a => OPTarget -> Maybe a -> Surreal m Value
  -- | Insert one or multiple records in a table
  Insert :: ToParam a => TableName -> [a] -> Surreal m [Value]
  -- | Update a specified record or table with the given value,
  --if no value is given, simply trigger an update event on the target
  Update :: ToParam a => OPTarget -> Maybe a -> Surreal m Value
  -- | This method merges specified data into either all records in a table or a single record
  Merge :: ToParam a => OPTarget -> Maybe a -> Surreal m Value
  -- | Patch a record or table with the given JSON Patches
  Patch :: ToParam a => OPTarget -> [a] -> Surreal m Value
  -- | Delete a record or whole contents of a table
  Delete :: OPTarget -> Surreal m Value

makeEffect ''Surreal

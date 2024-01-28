# surreal-db
Haskell driver for [SurrealDB](https://surrealdb.com). Currently uses SurrealDB's [JSON RPC](https://docs.surrealdb.com/docs/integration/websocket)
connection model. The `query` procedure has been enhanced to be type-safe. See [Using query](##Using-query).

Tested and developed using SurrealDB v1.0.2

## Note
SurrealDB currently does not support starting a transaction and commiting or rolling it back on a different query. So keep in mind
that to use transactions, you must start and end the transactions within the same query. If SurrealDB ever begins to support this,
interesting things could be done with it.

## Surreal Effect
This is the basic effect that provides the driver functionality. The definition includes the main SurrealDB RPC methods
plus some extra functionality for dealing with live queries. You can find the definition and docs [here](/src/Database/Surreal/Effect.hs).

### RPC Effect Handler
`runSurrealRPC` is a handler that runs the `Surreal` effect using RPC connection to the server.

## Basic Usage
To use this library it is recommended that you place `Surreal` effect somewhere in your app's effect stack like this:
```haskell
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Database.Surreal.Examples where

import           ClassyPrelude              as P
import           Control.Exception
import           Data.Aeson
import           Data.Profunctor
import           Data.Row
import           Data.Row.Aeson             ()
import           Database.Surreal.Core
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Dynamic
import           Effectful.TH

newtype MyAppState
  = MyAppState { someOtherState :: Text }

data MyAppEffect :: Effect where
  GetState :: MyAppEffect m MyAppState
  LogMessage :: Text -> MyAppEffect m ()

makeEffect ''MyAppEffect

runMyAppEffectIO :: (IOE :> es) => Eff (MyAppEffect : es) a -> Eff es a
runMyAppEffectIO = interpret
  (\_ -> \case
      GetState -> return $ MyAppState "some dummy state"
      LogMessage t -> liftIO $ print t)

type AppEffs = [MyAppEffect, Surreal, Error RPCError, IOE]

runApp :: Eff AppEffs a -> IO a
runApp m = do
  eConn <- runEff $ runError $ connectRPC defaultConnectionInfo
  case eConn of
    Right cs -> do
      eR <- runEff $ runError $ runSurrealRPC cs $ runMyAppEffectIO m
      case eR of
        Right r -> return r
        Left a  -> handleErr a
    Left a -> handleErr a
  where
    handleErr (callStack, e) = do
      print e
      putStrLn $ pack $ prettyCallStack callStack
      throw e
```
Since RPC uses `JSON`, we import `Data.Aeson` to do the conversions. `Data.Row` and `Data.Row.Aeson` are from
[`row-types`](https://hackage.haskell.org/package/row-types) and
[`row-types-aeson`](https://hackage.haskell.org/package/row-types-aeson) packages.
The row types allow us to have type safe queries.

The main import for the library is `Database.Surreal.Core` which provides the necessary bindings.
`defaultConnectionInfo` allows us to connect to `0.0.0.0` on port `8000` using username `root` and password `root`,
on namespace `test` and database `test`. You will need to change those to fit your needs.

Then you can define your app functions and use them in your main:

```haskell
simpleQuery1 :: Eff AppEffs (Maybe Value)
simpleQuery1 = select (maybe (P.error "invalid identifier") (Table . TableName) (mkIdentifier "artist"))
```

The above example is not ideal of-cource, because it is hard to write, returns a `Value` which is not very reliable
and does not allow you to filter or restrict the number of the results return, which makes it pretty useless since it
will return the whole table! I only added functions like this because the RPC is there. What I recommend is using the
`sql` quasi-quoter with the `query` function to get full power of SurrealQL and get a type-safe `Query`, which you can
run with `query` function.

## Using query
### Basic query
To use the `query` function, you will need to define a query using `sql` quasi-quoter. The same query above would be like this:

```haskell
simpleQuery2 :: Eff AppEffs Value
simpleQuery2 = query () [sql| (SELECT * FROM artist) :: Value; |]
```

The result of queries is defined by the result of the last expression in the query.
SurrealDB returns a result for each and every expression, but it seems to me that it just adds noise to the results.
So currently the library only returns the last result. This might change in the future if it becomes unavoidable.
So to give the type of the query result, we annotate the last expression with the result we expect.
Note that now the result of the query is `Value`, which means it could be `Null` (`null` JSON value, defined by `Aeson`).
There are multiple ways to fix that if getting a `Value` is undesirable:

### Type Annotations
Several parts of the queries support type annotations:
- selector fields in select expressions: `select name :: Text, company_name :: (Maybe Text) from users`
- expressions as a whole: `(select name, company_name from users) :: (Vector MyTypeSyn)`

Simple types like `Text` or `Bool` don't require parens but constructed types like `(Maybe Text)` do require them.

### Adding Type Annotations To Fields
Instead of the wildcard selector (`*`), we can be explicit about fields and add type annotations to each field:

```haskell
simpleQuery3 :: Eff AppEffs [Rec ("id" .== RecordID .+ "first_name" .== Text .+ "company_name" .== Maybe Text)]
simpleQuery3 = query () [sql|
                            SELECT id :: RecordID, first_name :: Text, company_name :: (Maybe Text)
                            FROM artist;
                            |]
```

If we give the fields type annotations, the field names and types will be combined to give us a `Rec` type.
This will now give us the type `[Rec ("id" .== RecordID .+ "first_name" .== Text .+ "company_name" .== Maybe Text)]`.

### Adding Type Annotations To Expressions
Lets say our `artist` table has `id`, `first_name`, and `company_name`. To make things easier for ourself,
we can define a type synonym and use that in our queries and as the result types of the `MyApp` monad:

```haskell
type Artist = Rec ("id" .== RecordID .+ "first_name" .== Text .+ "company_name" .== Maybe Text)

simpleQuery4 :: Eff AppEffs (Vector Artist)
simpleQuery4 = query () [sql| (SELECT * FROM artist) :: (Vector Artist); |]
```

This will make things clean and readable. Note that instead of annotating the fields, we have now annotated the
type of the expression as a whole.

### Adding Parameters To Queries
Parameters in SurrealQL use `$` prefix. Which means the value of `$some_param` could be defined
in the query itself or be supplied by SurrealDB or by the user making the query. We use `%` prefix
to be explicit about the parameters that the query expects from the user since that affects our
`Query` type that is returned by the `sql` quasi-quoter. For example:

```haskell
simpleQuery5 :: Eff AppEffs (Vector Artist)
simpleQuery5 = query (#last_name .== "Bekran")
  [sql|
      let $my_param = 123;
      (SELECT * FROM artist
      WHERE last_name = %last_name :: Text && some_other_field = $my_param) :: (Vector Artist);
      |]
```

In the example above, we define `$my_param` as a normal SQL param and it does not require a type annotation
to use since it is not supplied from haskell side, but `%last_name` is type annotated and prefixed with `%`
which means it is supposed to be supplied from haskell side and therefore must be present in the input params
of the `query` function. The input type of the `Query` returned by the `sql` quasi-quoter is a `Rec` of all
the input params present in the query. The input params must be always type annotated.

## Live Queries

To start a live query, you run a `live select` expression, which gives you a `UUID` (currently in text format)
which you use with `registerLiveListener` function to register a listener, which will be called whenever
a notification is received from the server. The type of the handler is `LiveResponse -> IO ()`. See below for an example.

To remove a listener and kill the live query, use `unregisterLiveListener` with the `UUID` you received when running
the `select live` query.

```haskell
testLiveQuery :: Eff AppEffs ()
testLiveQuery = do
  let q = [sql|
              live select * from artist;
              |]
  uuid <- query () q
  listenMvar <- newEmptyMVar
  let handler = putMVar listenMvar
  print uuid
  registerLiveListener uuid handler
  _ <- forever $ do
    r <- takeMVar listenMvar
    putStrLn $ "received live notification: " <> tshow r
  return ()
```

## Other Examples
The [Examples.hs](/src/Database/Surreal/Examples.hs) file contains many other useful examples.


{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
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
import           Effectful.Concurrent
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
  eConn <- runEff $ runConcurrent $ runError $ connectRPC defaultConnectionInfo
  case eConn of
    Right cs -> do
      eR <- runEff $ runError $ runSurrealRPC cs $ runMyAppEffectIO m
      case eR of
        Right r -> return r
        Left a  -> handleErr a
    Left a -> handleErr a
  where
    handleErr :: (CallStack, RPCError) -> IO a
    handleErr (callStack, e) = do
      print e
      putStrLn $ pack $ prettyCallStack callStack
      throw e

simpleQuery1 :: Eff AppEffs (Maybe Value)
simpleQuery1 = select (maybe (P.error "invalid identifier") (Table . TableName) (mkIdentifier "artist"))

simpleQuery2 :: Eff AppEffs Value
simpleQuery2 = query () [sql| (SELECT * FROM artist) :: Value; |]

simpleQuery3 :: Eff AppEffs [Rec ("id" .== RecordID .+ "first_name" .== Text .+ "company_name" .== Maybe Text)]
simpleQuery3 = query () [sql|
                            SELECT id :: RecordID, first_name :: Text, company_name :: (Maybe Text)
                            FROM artist;
                            |]

type Artist = Rec ("id" .== RecordID .+ "first_name" .== Text .+ "company_name" .== Maybe Text)

simpleQuery4 :: Eff AppEffs (Vector Artist)
simpleQuery4 = query () [sql| (SELECT * FROM artist) :: (Vector Artist); |]

simpleQuery5 :: Eff AppEffs (Vector Artist)
simpleQuery5 = query (#last_name .== "Bekran")
  [sql|
      let $my_param = 123;
      (SELECT * FROM artist
      WHERE last_name = %last_name :: Text && some_other_field = $my_param) :: (Vector Artist);
      |]

type TestRecType = Rec ("category" .== Text)
type TestRecType2 = Rec ("id" .== RecordID .+ "cat" .== Vector TestRecType)
type TestRecType3 = Rec ("id" .== RecordID .+ "name" .== Text .+ "fname" .== Maybe Text)

test :: Eff AppEffs [Int]
test = do
  let q =
        [sql|
            select id :: (Text), ->create->product AS cat :: (Vector TestRecType)
            from artist:00b2pg847d7b8r08t08t..
            where name = %name :: (Text) && fname = %fname :: (Int64)
            limit 2
            fetch cat;
            select 1 + 2 as ppp :: (Int) from artist limit 1;
            |]
      q' = dimap (\(txt, i) -> #name .== txt .+ #fname .== i) (\r -> map (.! #ppp) <$> r) q
  putStr (getSQL q')
  query ("my-name", 123) q'

testLiveQuery :: Eff AppEffs ()
testLiveQuery = do
  let q = [sql|
              live select * from artist;
              |]
  uuid <- query () q
  listenMvar <- P.newEmptyMVar
  let handler = P.putMVar listenMvar
  print uuid
  registerLiveListener uuid handler
  --unregisterLiveListener uuid
  --_ <- tryAny $ unregisterLiveListener "invalid uuid" -- this causes an error on db side
  _ <- forever $ do
    r <- P.takeMVar listenMvar
    putStrLn $ "received live notification: " <> tshow r
  return ()

test2 :: Eff AppEffs ()
test2 = do
  let q =
        [sql|
            (select *, ->create->product AS cat
            from artist
            where id = %id :: (RecordID)
            limit 2
            fetch cat) :: (Vector TestRecType2);
            |]
  let tn = maybe (P.error "invalid identifier") TableName (mkIdentifier "artist")
  let rid = RecordID tn (TextID "00b2pg847d7b8r08t08t")
  putStr (getSQL q)
  query (#id .== rid) q >>= print

insertTest :: Eff AppEffs ()
insertTest = do
  let q =
        [sql|
            (INSERT INTO test (id, name, fname)
            VALUES (test:uuid(), %v1 :: Text, %v2 :: Text), ("test:farzad2", "farzad2", "bekran2")
            ON DUPLICATE KEY UPDATE numUpdate += 1) :: (Vector TestRecType3);
            |]
  query (#v1 .== "inputval1" .+ #v2 .== "inputval \" 2") q >>= print

insertTest2 :: Eff AppEffs ()
insertTest2 = do
  let q =
        [sql|
            INSERT INTO test { name : %v1 :: Text, fname : %v2 :: Text } :: (Vector TestRecType3);
            |]
  query (#v1 .== "v1" .+ #v2 .== "v2") q >>= print

insertTest3 :: Eff AppEffs ()
insertTest3 = do
  let q =
        [sql|
            INSERT INTO test [ {id : test:uuid(), name : "farzad", fname : "bekran" }
                             , { name : "farzad2", fname : "bekran2" }] :: (Vector TestRecType3);
            |]
  query () q >>= print

createTest1 :: Eff AppEffs ()
createTest1 = do
  let q =
        [sql|
            (CREATE test CONTENT {name: "farzad create", fname: "bekran"}) :: (Vector TestRecType3);
            |]
  query () q >>= print

createTest2 :: Eff AppEffs ()
createTest2 = do
  let q =
        [sql|
            (CREATE test SET name = "farzad create2", fname = "bekran") :: (Vector TestRecType3);
            |]
  query () q >>= print

createTest3 :: Eff AppEffs ()
createTest3 = do
  let q =
        [sql|
            (CREATE test SET name = "farzad create3", fname = "bekran" RETURN id, name, fname) :: (Vector TestRecType3);
            |]
  query () q >>= print

deleteTest1 :: Eff AppEffs ()
deleteTest1 = query () [sql| DELETE test :: (); |]

deleteTest2 :: Eff AppEffs ()
deleteTest2 = do
  let rid = RecordID
        (TableName $ fromMaybe (P.error "invalid id") (mkIdentifier "test"))
        (TextID "018c4414-8cf3-7e36-a5c8-f341ac9749c2")
  let q = [sql| (DELETE test where id = %id :: RecordID) :: (); |]
  query (#id .== rid) q

updateTest1 :: Eff AppEffs ()
updateTest1 = do
  let rid = RecordID
        (TableName $ fromMaybe (P.error "invalid id") (mkIdentifier "test"))
        (TextID "018c443e-9f59-75f0-848f-dcd2417eb275")
  -- need the parens around (%id :: RecordID) to avoid confusing RETURN keyword with a type
  let q = [sql| (UPDATE test SET name = "updated Name" where id = (%id :: RecordID) RETURN NONE) :: (); |]
  query (#id .== rid) q

updateTest2 :: Eff AppEffs ()
updateTest2 = do
  let q = [sql| (UPDATE artist:00d3xv269u0x5o37q16u->create->product SET name = "updated Name") :: (); |]
  query () q

selectTest1 :: Eff AppEffs ()
selectTest1 = do
  let q = [sql| (SELECT * FROM person WHERE ->knows->person->(knows WHERE influencer = %v1 :: Bool) TIMEOUT 5s) :: Value; |]
  query (#v1 .== True) q >>= print

relateTest1 :: Eff AppEffs ()
relateTest1 = do
  let q = [sql|
              (RELATE person:l19zjikkw1p1h9o6ixrg->wrote->article:8nkk6uj4yprt49z7y3zm
              SET time.written = time::now()) :: Value;
              |]
  query () q >>= print

defineTest1 :: Eff AppEffs ()
defineTest1 = do
  let q =
        [sql|
            DEFINE NAMESPACE test;
            DEFINE DATABASE test;
            DEFINE USER farzad ON NAMESPACE PASSWORD 'farzad' ROLES viewer, editor;
            DEFINE USER farzad ON DATABASE PASSHASH 'farzad' ROLES owner;
            DEFINE USER farzad ON ROOT PASSHASH 'farzad' ROLES owner;
            DEFINE TOKEN token_name ON NAMESPACE TYPE EDDSA VALUE '123';
            DEFINE TOKEN token_name ON SCOPE scope_name TYPE EDDSA VALUE '123';
            DEFINE SCOPE scope_name;
            DEFINE SCOPE scope_name SESSION 24h;
            DEFINE SCOPE scope_name SESSION 24h SIGNUP (select * from test);
            DEFINE SCOPE scope_name SESSION 24h SIGNUP (select * from test) SIGNIN (select * from test2);
            DEFINE TABLE test DROP;
            DEFINE TABLE test SCHEMAFULL;
            DEFINE TABLE test SCHEMALESS;
            DEFINE TABLE test SCHEMAFULL AS Select * from test2 WHERE f1 = 1 group by f2;
            DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2) PERMISSIONS NONE;
            DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2) PERMISSIONS FULL;
            DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2)
              CHANGEFEED 1h
              PERMISSIONS
                FOR select,create WHERE f1 = 1
                FOR delete WHERE (f2 = 2) OR (f3 = 3);
            DEFINE EVENT my_event ON test WHEN a = 1 THEN b = 2;
            DEFINE EVENT create_rej ON users WHEN $before.profile != $after.profile THEN {
              IF ((SELECT VALUE id FROM profiles) CONTAINSNOT $after.profile) {
                throw "invalid profile id!";
              }};
            DEFINE FIELD my_field ON TABLE test;
            DEFINE FIELD my_field ON TABLE test FLEXIBLE TYPE option<string>;
            DEFINE FIELD my_field ON TABLE test FLEXIBLE TYPE option<string>
              VALUE 1
              ASSERT f1 > 10
              PERMISSIONS
                FOR select,create WHERE f1 = 1
                FOR delete WHERE (f2 = 2) OR (f3 = 3);
            DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS lowercase;
            DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS edgengram(3,10);
            DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS snowball(english);
            DEFINE INDEX my_index ON test FIELDS f1,f2 UNIQUE;
            DEFINE INDEX my_index ON test FIELDS f1,f2 SEARCH ANALYZER my_analyzer BM25;
            DEFINE INDEX my_index ON test FIELDS f1,f2 SEARCH ANALYZER my_analyzer BM25(0.1,0.2);
            DEFINE FIELD permissions.* ON TABLE acl TYPE string;
            (SELECT field1.* from test) :: Value;
            SELECT %p :: (Text) from test where $param.field[1] = 1;
            (create test_table_2 set p.testField = /[A-Z]/) :: Value;
            |]
  query (#p .== "my val") q >>= print

inputTest :: Eff AppEffs ()
inputTest = do
  let q =
        [sql|
            select id :: RecordID, first_name :: Text, last_name :: Text
            from artist
            where first_name = %fn :: Text;
            |]
  query (#fn .== "Lasonya") q >>= print

returnTest :: Eff AppEffs ()
returnTest = do
  let q =
        [sql| let $r = select id :: Text, ->create->product AS cat :: (Vector TestRecType)
                       from artist:00b2pg847d7b8r08t08t..
                       fetch cat;
              return $r :: (Vector TestRecType2);
            |]
  query () q >>= print

exceptionTest :: Eff AppEffs ()
exceptionTest = query () [sql| throw "my error!"; |]

txTest :: Eff AppEffs ()
txTest = do
  let q = [sql|
              begin;
              (INSERT INTO test { name : "tx-farzad", fname : "tx-bekran" }) :: (Vector Value);
              LET $res = select id from test limit 1;
              commit;
              return $res :: Value;
              |]
  query () q >>= print

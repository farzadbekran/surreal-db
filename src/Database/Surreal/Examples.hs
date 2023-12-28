{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Database.Surreal.Examples where

import           ClassyPrelude         as P
import           Data.Aeson
import           Data.Profunctor
import           Data.Row
import           Data.Row.Aeson        ()
import           Database.Surreal.Core

newtype MyAppState
  = MyAppState { someOtherState :: Text }

type MyApp a
  = ReaderT MyAppState (SurrealRPCT IO) a

runMyApp :: MyApp a -> IO a
runMyApp m = do
  cs <- connectRPC defaultConnectionInfo
  let appState = MyAppState "some other state"
  runSurrealRPCT cs (runReaderT m appState)

main :: IO ()
main = catch
  (do
      r <- runMyApp test
      print r)
  (\(e :: SomeException) -> putStrLn $ "cought exception: " <> pack (displayException e))

type TestRecType = Rec ("category" .== Text)
type TestRecType2 = Rec ("id" .== RecordID .+ "cat" .== Vector TestRecType)
type TestRecType3 = Rec ("id" .== RecordID .+ "name" .== Text .+ "fname" .== Maybe Text)

test :: MyApp [Int]
test = do
  let q =
        [sql|
            select id :: Text, ->create->product AS cat :: (Vector TestRecType)
            from artist:00b2pg847d7b8r08t08t..
            where name = %name :: Text && fname = %fname :: Int64
            limit 2
            fetch cat;
            select 1 + 2 as ppp :: Int from artist limit 1;
            |]
      q' = dimap (\(txt, i) -> #name .== txt .+ #fname .== i) (\r -> map (.! #ppp) <$> r) q
  putStr (getSQL q')
  query ("my-name", 123) q'

testLiveQuery :: MyApp ()
testLiveQuery = do
  let q = [sql|
              live select * from artist;
              |]
  uuid <- query () q
  listenMvar <- newEmptyMVar
  let handler = putMVar listenMvar
  print uuid
  registerLiveListener uuid handler
  --unregisterLiveListener uuid
  _ <- tryAny $ unregisterLiveListener "invalid uuid" -- this causes an error on db side
  _ <- forever $ do
    r <- takeMVar listenMvar
    putStrLn $ "received live notification: " <> tshow r
  return ()

test2 :: MyApp ()
test2 = do
  let q =
        [sql|
            (select *, ->create->product AS cat
            from artist
            where id = %id :: RecordID
            limit 2
            fetch cat) :: (Vector TestRecType2);
            |]
  let tn = maybe (P.error "invalid identifier") TableName (mkIdentifier "artist")
  let rid = RecordID tn (TextID "00b2pg847d7b8r08t08t")
  putStr (getSQL q)
  query (#id .== rid) q >>= print

insertTest :: MyApp ()
insertTest = do
  let q =
        [sql|
            (INSERT INTO test (id, name, fname)
            VALUES (test:uuid(), %v1 :: Text, %v2 :: Text), ("test:farzad2", "farzad2", "bekran2")
            ON DUPLICATE KEY UPDATE numUpdate += 1) :: Vector TestRecType3;
            |]
  query (#v1 .== "inputval1" .+ #v2 .== "inputval \" 2") q >>= print

insertTest2 :: MyApp ()
insertTest2 = do
  let q =
        [sql|
            INSERT INTO test { name : %v1 :: Text, fname : %v2 :: Text } :: Vector TestRecType3;
            |]
  query (#v1 .== "v1" .+ #v2 .== "v2") q >>= print

insertTest3 :: MyApp ()
insertTest3 = do
  let q =
        [sql|
            INSERT INTO test [{id : test:uuid(), name : "farzad", fname : "bekran" },{ name : "farzad2", fname : "bekran2" }] :: Vector TestRecType3;
            |]
  query () q >>= print

createTest1 :: MyApp ()
createTest1 = do
  let q =
        [sql|
            (CREATE test CONTENT {name: "farzad create", fname: "bekran"}) :: Vector TestRecType3;
            |]
  query () q >>= print

createTest2 :: MyApp ()
createTest2 = do
  let q =
        [sql|
            (CREATE test SET name = "farzad create2", fname = "bekran") :: Vector TestRecType3;
            |]
  query () q >>= print

createTest3 :: MyApp ()
createTest3 = do
  let q =
        [sql|
            (CREATE test SET name = "farzad create3", fname = "bekran" RETURN id, name, fname) :: Vector TestRecType3;
            |]
  query () q >>= print

deleteTest1 :: MyApp ()
deleteTest1 = query () [sql| DELETE test :: (); |]

deleteTest2 :: MyApp ()
deleteTest2 = do
  let rid = RecordID
        (TableName $ fromMaybe (P.error "invalid id") (mkIdentifier "test"))
        (TextID "018c4414-8cf3-7e36-a5c8-f341ac9749c2")
  let q = [sql| (DELETE test where id = %id :: RecordID) :: (); |]
  query (#id .== rid) q

updateTest1 :: MyApp ()
updateTest1 = do
  let rid = RecordID
        (TableName $ fromMaybe (P.error "invalid id") (mkIdentifier "test"))
        (TextID "018c443e-9f59-75f0-848f-dcd2417eb275")
  -- need the parens around (%id :: RecordID) to avoid confusing RETURN keyword with a type
  let q = [sql| (UPDATE test SET name = "updated Name" where id = (%id :: RecordID) RETURN NONE) :: (); |]
  query (#id .== rid) q

updateTest2 :: MyApp ()
updateTest2 = do
  let q = [sql| (UPDATE artist:00d3xv269u0x5o37q16u->create->product SET name = "updated Name") :: (); |]
  query () q

selectTest1 :: MyApp ()
selectTest1 = do
  let q = [sql| (SELECT * FROM person WHERE ->knows->person->(knows WHERE influencer = %v1 :: Bool) TIMEOUT 5s) :: Value; |]
  query (#v1 .== True) q >>= print

relateTest1 :: MyApp ()
relateTest1 = do
  let q = [sql|
              (RELATE person:l19zjikkw1p1h9o6ixrg->wrote->article:8nkk6uj4yprt49z7y3zm
              SET time.written = time::now()) :: Value;
              |]
  query () q >>= print

defineTest1 :: MyApp ()
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
            SELECT %p :: Text from test where $param.field[1] = 1;
            (create test_table_2 set p.testField = /[A-Z]/) :: Value;
            |]
  query (#p .== "my val") q >>= print

inputTest :: MyApp ()
inputTest = do
  let q =
        [sql|
            select id :: RecordID, first_name :: Text, last_name :: Text
            from artist
            where first_name = %fn :: Text;
            |]
  query (#fn .== "Lasonya") q >>= print

returnTest :: MyApp ()
returnTest = do
  let q =
        [sql| let $r = select id :: Text, ->create->product AS cat :: (Vector TestRecType)
                       from artist:00b2pg847d7b8r08t08t..
                       fetch cat;
              return $r :: Vector TestRecType2;
            |]
  query () q >>= print

exceptionTest :: MyApp ()
exceptionTest = query () [sql| throw "my error!"; |]

txTest :: MyApp ()
txTest = do
  let q = [sql|
              begin;
              (INSERT INTO test { name : "tx-farzad", fname : "tx-bekran" }) :: Vector Value;
              LET $res = select id from test limit 1;
              commit;
              return $res :: Value;
              |]
  query () q >>= print

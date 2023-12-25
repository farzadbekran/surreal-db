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

module Database.Surreal.ManualTest where

import           ClassyPrelude         as P
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
            -- throw "asdasd";
            live select * from artist;
            info for ns;
            info for db;
            info for root;
            info for scope my_scope;
            info for table my_table;
            --kill $qid;
            remove event my_event on table my_table;
            select 1 + 2 as ppp :: Int from artist limit 1;
            |]
      q2 = lmap (\(txt, i) -> #name .== txt .+ #fname .== i) q
      q3 = rmap (\r -> map (.! #ppp) <$> r) q2
  putStr (getSQL q)
  --runQuery (#name .== "my-name" .+ #fname .== 123) q
  runQuery ("my-name", 123) q3

testLiveQuery :: MyApp ()
testLiveQuery = do
  let q = [sql|
              live select * from artist;
              |]
  uuid <- runQuery () q
  listenMvar <- newEmptyMVar
  let handler = putMVar listenMvar
  print uuid
  registerLiveListener uuid handler
  --unregisterLiveListener uuid
  _ <- tryAny $ unregisterLiveListener "test" -- this causes an error on db side
  _ <- forever $ do
    r <- takeMVar listenMvar
    putStrLn $ "received live notification: " <> tshow r
  return ()

-- test2 :: IO ()
-- test2 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let rid = RecordID (TableName "artist") (TextID "00b2pg847d7b8r08t08t")
--     let q@(Query t _ _) =
--           [sql|
--               (
--               select *, ->create->product AS cat
--               from artist
--               where id = %id :: RecordID
--               limit 2
--               fetch cat
--               ) :: (Vector TestRecType2);
--               |]
--     putStr t
--     runQuery (#id .== rid) q
--   print res

-- insertTest :: IO ()
-- insertTest = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (INSERT INTO test (id, name, fname) VALUES (test:uuid(), %v1 :: Text, %v2 :: Text), ("test:farzad2", "farzad2", "bekran2")
--                 ON DUPLICATE KEY UPDATE numUpdate += 1) :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery (#v1 .== "inputval1" .+ #v2 .== "inputval \" 2") q
--   print res

-- insertTest2 :: IO ()
-- insertTest2 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               INSERT INTO test { name : "farzad", fname : "bekran" } :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- insertTest3 :: IO ()
-- insertTest3 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               INSERT INTO test [{id : test:uuid(), name : "farzad", fname : "bekran" },{ name : "farzad2", fname : "bekran2" }] :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- createTest1 :: IO ()
-- createTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (CREATE test CONTENT {name: "farzad create", fname: "bekran"}) :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- createTest2 :: IO ()
-- createTest2 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (CREATE test SET name = "farzad create2", fname = "bekran") :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- createTest3 :: IO ()
-- createTest3 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (CREATE test SET name = "farzad create3", fname = "bekran" RETURN id, name, fname) :: Vector TestRecType3;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- deleteTest1 :: IO ()
-- deleteTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (DELETE test) :: ();
--               |]
--     putStr t
--     runQuery () q
--   print res

-- deleteTest2 :: IO ()
-- deleteTest2 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let rid = RecordID (TableName "test") (TextID "018c4414-8cf3-7e36-a5c8-f341ac9749c2")
--     print rid
--     let q@(Query t _ _) =
--           [sql|
--               (DELETE test where id = %id :: RecordID) :: ();
--               |]
--     putStr t
--     runQuery (#id .== rid) q
--   print res

-- updateTest1 :: IO ()
-- updateTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let rid = RecordID (TableName "test") (TextID "018c443e-9f59-75f0-848f-dcd2417eb275")
--     print rid
--     let q@(Query t _ _) =
--           [sql|
--               (UPDATE test SET name = "updated Name" where id = (%id :: RecordID) RETURN NONE) :: ();
--               |]
--     putStr t
--     runQuery (#id .== rid) q
--   print res

-- updateTest2 :: IO ()
-- updateTest2 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (UPDATE artist:00d3xv269u0x5o37q16u->create->product SET name = "updated Name") :: ();
--               |]
--     putStr t
--     runQuery () q
--   print res

-- selectTest1 :: IO ()
-- selectTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (SELECT * FROM person WHERE ->knows->person->(knows WHERE influencer = true) TIMEOUT 5s) :: Value;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- relateTest1 :: IO ()
-- relateTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               (RELATE person:l19zjikkw1p1h9o6ixrg->wrote->article:8nkk6uj4yprt49z7y3zm SET time.written = "just now!") :: Value;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- defineTest1 :: IO ()
-- defineTest1 = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               DEFINE NAMESPACE test;
--               DEFINE DATABASE test;
--               DEFINE USER farzad ON NAMESPACE PASSWORD 'farzad' ROLES viewer, editor;
--               DEFINE USER farzad ON DATABASE PASSHASH 'farzad' ROLES owner;
--               DEFINE USER farzad ON ROOT PASSHASH 'farzad' ROLES owner;
--               DEFINE TOKEN token_name ON NAMESPACE TYPE EDDSA VALUE '123';
--               DEFINE TOKEN token_name ON SCOPE scope_name TYPE EDDSA VALUE '123';
--               DEFINE SCOPE scope_name;
--               DEFINE SCOPE scope_name SESSION 24h;
--               DEFINE SCOPE scope_name SESSION 24h SIGNUP (select * from test);
--               DEFINE SCOPE scope_name SESSION 24h SIGNUP (select * from test) SIGNIN (select * from test2);
--               DEFINE TABLE test DROP;
--               DEFINE TABLE test SCHEMAFULL;
--               DEFINE TABLE test SCHEMALESS;
--               DEFINE TABLE test SCHEMAFULL AS Select * from test2 WHERE f1 = 1 group by f2;
--               DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2) PERMISSIONS NONE;
--               DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2) PERMISSIONS FULL;
--               DEFINE TABLE test SCHEMAFULL AS (Select * from test2 WHERE f1 = 1 group by f2)
--                 CHANGEFEED 1h
--                 PERMISSIONS
--                   FOR select,create WHERE f1 = 1
--                   FOR delete WHERE (f2 = 2) OR (f3 = 3);
--               DEFINE EVENT my_event ON test WHEN a = 1 THEN b = 2;
--               DEFINE FIELD my_field ON TABLE test;
--               DEFINE FIELD my_field ON TABLE test FLEXIBLE TYPE option<string>;
--               DEFINE FIELD my_field ON TABLE test FLEXIBLE TYPE option<string>
--                 VALUE 1
--                 ASSERT f1 > 10
--                 PERMISSIONS
--                   FOR select,create WHERE f1 = 1
--                   FOR delete WHERE (f2 = 2) OR (f3 = 3);
--               DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS lowercase;
--               DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS edgengram(3,10);
--               DEFINE ANALYZER my_analyzer TOKENIZERS blank FILTERS snowball(english);
--               DEFINE INDEX my_index ON test FIELDS f1,f2 UNIQUE;
--               DEFINE INDEX my_index ON test FIELDS f1,f2 SEARCH ANALYZER my_analyzer BM25;
--               DEFINE INDEX my_index ON test FIELDS f1,f2 SEARCH ANALYZER my_analyzer BM25(0.1,0.2);
--               DEFINE FIELD permissions.* ON TABLE acl TYPE string;
--               (SELECT field1.* from test) :: Value;
--               SELECT %p :: Text from test where $param.field[1] = 1;
--               (create test_table_2 set p.testField = /[A-Z]/) :: Value;
--               |]
--     putStr t
--     runQuery (#p .== "my val") q
--   print res

-- inputTest :: IO ()
-- inputTest = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               select id :: RecordID, first_name :: Text, last_name :: Text
--               from artist
--               where first_name = %fn :: Text;
--               |]
--     putStr t
--     runQuery (#fn .== "Lasonya") q
--   print res

-- returnTest :: IO ()
-- returnTest = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               return 123 :: Int64;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- exceptionTest :: IO ()
-- exceptionTest = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q@(Query t _ _) =
--           [sql|
--               return 123 :: Int64;
--               |]
--     putStr t
--     runQuery () q
--   print res

-- txTest :: IO ()
-- txTest = do
--   connState <- connect defaultConnectionInfo
--   res <- runSurreal connState $ do
--     let q =
--           [sql|
--               begin;
--               INSERT INTO test { name : "tx-farzad", fname : "tx-bekran" } :: Vector Value;
--               LET $res = select id from test limit 1;
--               commit;
--               return $res :: Value;
--               |]
--     -- _ <- runQuery () q
--     -- runQuery () [sql| commit; |]
--     runQuery () q
--   print res

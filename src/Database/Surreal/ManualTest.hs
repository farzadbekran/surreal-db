{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Database.Surreal.ManualTest where

import           ClassyPrelude                   as P
import           Data.Aeson
import           Data.Aeson.KeyMap
import           Data.Row
import           Data.Row.Aeson                  ()
import           Database.Surreal.AST            ( ID (TextID),
                                                   RecordID (RecordID),
                                                   TableName (TableName) )
import           Database.Surreal.ASTJSON        ()
import           Database.Surreal.TH
import           Database.Surreal.WS.RPC.Surreal as RPC

type TestRecType = Rec ("category" .== Text)
type TestRecType2 = Rec ("id" .== RecordID .+ "cat" .== Vector TestRecType)
type TestRecType3 = Rec ("id" .== RecordID .+ "name" .== Text .+ "fname" .== Maybe Text)

test :: IO ()
test = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              select id :: Text, ->create->product AS cat :: (Vector TestRecType)
              from artist:00b2pg847d7b8r08t08t..
              --where name = %1 :: Text && fname = %2 :: Int64
              limit 2
              fetch cat;
              --select 1 + 2 as ppp :: Int from artist limit 1;
              |]
    print t
    runQuery () q
  print res

test2 :: IO ()
test2 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let rid = RecordID (TableName "artist") (TextID "00b2pg847d7b8r08t08t")
    let q@(Query t _ _) =
          [sql|
              (select *, ->create->product AS cat
              from artist where id = %1 :: RecordID
              limit 2
              fetch cat) :: (Vector TestRecType2);
              |]
    print t
    runQuery rid q
  print res

insertTest :: IO ()
insertTest = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (INSERT INTO test (id, name, fname) VALUES (test:uuid(), %1 :: Text, %2 :: Text), ("test:farzad2", "farzad2", "bekran2")
                ON DUPLICATE KEY UPDATE numUpdate += 1) :: Vector TestRecType3;
              |]
    print t
    runQuery ("inputval1","inputval \" 2") q
  print res

insertTest2 :: IO ()
insertTest2 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              INSERT INTO test { name : "farzad", fname : "bekran" } :: Vector TestRecType3;
              |]
    print t
    runQuery () q
  print res

insertTest3 :: IO ()
insertTest3 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              INSERT INTO test [{id : test:uuid(), name : "farzad", fname : "bekran" },{ name : "farzad2", fname : "bekran2" }] :: Vector TestRecType3;
              |]
    print t
    runQuery () q
  print res

createTest1 :: IO ()
createTest1 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (CREATE test CONTENT {name: "farzad create", fname: "bekran"}) :: Vector TestRecType3;
              |]
    print t
    runQuery () q
  print res

createTest2 :: IO ()
createTest2 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (CREATE test SET name = "farzad create2", fname = "bekran") :: Vector TestRecType3;
              |]
    print t
    runQuery () q
  print res

createTest3 :: IO ()
createTest3 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (CREATE test SET name = "farzad create3", fname = "bekran" RETURN id, name, fname) :: Vector TestRecType3;
              |]
    print t
    runQuery () q
  print res

deleteTest1 :: IO ()
deleteTest1 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (DELETE test) :: ();
              |]
    print t
    runQuery () q
  print res

deleteTest2 :: IO ()
deleteTest2 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let rid = RecordID (TableName "test") (TextID "018c4414-8cf3-7e36-a5c8-f341ac9749c2")
    print rid
    let q@(Query t _ _) =
          [sql|
              (DELETE test where id = %1 :: RecordID) :: ();
              |]
    print t
    runQuery rid q
  print res

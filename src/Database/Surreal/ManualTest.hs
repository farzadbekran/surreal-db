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
import           Database.Surreal.TH
import           Database.Surreal.WS.RPC.Surreal as RPC

type TestRecType = Rec ("category" .== Text)
type TestRecType2 = Rec ("id" .== Text .+ "name" .== Text .+ "fname" .== Text)

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

insertTest :: IO ()
insertTest = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              (INSERT INTO test (id,name,fname) VALUES ("test:farzad","farzad","bekran")
                ON DUPLICATE KEY UPDATE numUpdate += 1) :: TestRecType2;
              |]
    print t
    runQuery () q
  print res

insertTest2 :: IO ()
insertTest2 = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [sql|
              INSERT INTO test {"name" : "farzad", "fname" : "bekran"} :: TestRecType2;
              |]
    print t
    runQuery () q
  print res

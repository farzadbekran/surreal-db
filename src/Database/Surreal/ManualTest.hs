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

test :: IO ()
test = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q@(Query t _ _) =
          [query|
                select id :: Text,
                       ->create->product AS cat :: (Vector TestRecType)
                from artist
                limit 2
                fetch cat
                |]
    print t
    runQuery () q
  print res

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Surreal.ManualTest where

import           ClassyPrelude                   as P
import           Data.Aeson
import           Database.Surreal.TH
import           Database.Surreal.WS.RPC.Surreal as RPC

test :: IO ()
test = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    signinRes <- RPC.send "signin" [object [ "user" .= String "root"
                                           , "pass" .= String "root"
                                           , "ns" .= String "test"
                                           , "db" .= String "test"]]
    traceM $ pack $ show signinRes
    let q = [query|select first_name, last_name from artist limit 10|]
    runQuery () q
  print res

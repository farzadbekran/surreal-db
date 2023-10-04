{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications     #-}

module Database.Surreal.ManualTest where

import           ClassyPrelude                   as P
import           Data.Aeson
import           Database.Surreal.TH
import           Database.Surreal.WS.RPC.Surreal as RPC
import           Data.Row.Aeson ()

test :: IO ()
test = do
  connState <- RPC.connect RPC.defaultConnectionInfo
  res <- RPC.runSurreal connState $ do
    let q = [query|select first_name :: Text, last_name :: Text from artist limit 1|]
    runQuery () q
  print res

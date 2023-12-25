{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Database.Surreal.Core
    ( module Data.Row.Aeson
    , module Database.Surreal.AST
    , module Database.Surreal.ASTJSON
    , module Database.Surreal.MonadSurreal
    , module Database.Surreal.TH
    , module Database.Surreal.Types
    , module Database.Surreal.WS.RPC
    , module Database.Surreal.WS.RPCTypes
    ) where

import           Data.Row.Aeson                ()
import           Database.Surreal.AST
import           Database.Surreal.ASTJSON      ()
import           Database.Surreal.MonadSurreal
import           Database.Surreal.TH
import           Database.Surreal.Types
import           Database.Surreal.WS.RPC
import           Database.Surreal.WS.RPCTypes

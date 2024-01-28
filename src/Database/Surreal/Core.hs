{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Database.Surreal.Core
    ( module Data.Row.Aeson
    , module Database.Surreal.AST
    , module Database.Surreal.ASTJSON
    , module Database.Surreal.Effect
    , module Database.Surreal.Effect.Handlers.RPC
    , module Database.Surreal.Effect.Handlers.RPCTypes
    , module Database.Surreal.TH
    , module Database.Surreal.Types
    ) where

import           Data.Row.Aeson                            ()
import           Database.Surreal.AST
import           Database.Surreal.ASTJSON                  ()
import           Database.Surreal.Effect
import           Database.Surreal.Effect.Handlers.RPC
import           Database.Surreal.Effect.Handlers.RPCTypes
import           Database.Surreal.TH
import           Database.Surreal.Types

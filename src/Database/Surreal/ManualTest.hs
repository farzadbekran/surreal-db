{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Surreal.ManualTest where

import ClassyPrelude
import Data.Kind (Type)
import GHC.TypeLits
import Data.Proxy
import Data.Row
import GHC.OverloadedLabels

data SomeLabel where
  SomeLabel :: KnownSymbol s => Label s -> SomeLabel

mkSymbol :: String -> SomeSymbol
mkSymbol s = case someSymbolVal s of
  SomeSymbol (_ :: Proxy t) -> SomeSymbol (Proxy :: Proxy t)


{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Surreal.ASTJSON where

import           ClassyPrelude
import           Data.Aeson              as J
import           Data.Aeson.Types        ( parseFail, prependFailure,
                                           typeMismatch )
import           Database.Surreal.AST
import           Database.Surreal.Parser
import           Text.Megaparsec

instance FromJSON RecordID where
  parseJSON = \case
    J.String s -> do
      let rid = parse recordID "" $ unpack s
      case rid of
        Right ast -> return ast
        Left e    -> parseFail $ errorBundlePretty e
    a -> prependFailure "parsing RecordID failed, "
      (typeMismatch "Object" a)

instance ToJSON RecordID where
  toJSON rid = String $ toQL rid

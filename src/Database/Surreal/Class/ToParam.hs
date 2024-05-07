{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Surreal.Class.ToParam where

import           ClassyPrelude

import           Data.Aeson.Encoding ( encodingToLazyByteString )
import           Data.Aeson.Types
import           Data.Row.Records    hiding ( map )
import           Data.Type.Equality
import           GHC.Generics

-- | a class to convert from haskell values to SurrealDB params.
-- we can't use ToJSON because it does not support values like NONE
-- or surrealDB expressions since they are not standard JSON values
class ToParam a where
  toParam :: a -> Text

instance {-# OVERLAPPING #-} Forall r ToParam => ToParam (Rec r) where
  toParam r = let fieldList = eraseWithLabels @ToParam toParam r in
    "{" <> intercalate "," (map getField fieldList) <> "}"
    where
      getField :: (String, Text) -> Text
      getField (label, val) = pack label <> ":" <> val

instance {-# OVERLAPPING #-} ToParam String where
  toParam s = pack $ "\"" <> s <> "\""

instance {-# OVERLAPPING #-} ToParam Text where
  toParam s = "\"" <> s <> "\""

-- | for any kind of list like data type (List, Vector, etc)
instance {-# OVERLAPPING #-} (ToParam a, Functor t, MonoFoldable (t Text), Element (t Text) ~ Text) => ToParam (t a) where
  toParam ta = "[" <> intercalate "," (toList $ fmap toParam ta) <> "]"

-- | using aeson's generic machinary to provide ToParam instances for Generic data types
instance {-# OVERLAPPABLE #-} (Generic a, GToJSON' Encoding Zero (Rep a)) => ToParam a where
  toParam a = toStrict $ decodeUtf8 $ encodingToLazyByteString $ genericToEncoding defaultOptions { omitNothingFields = True } a

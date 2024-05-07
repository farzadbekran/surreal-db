{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Surreal.Class.ToParam where

import           ClassyPrelude

import           Data.Aeson         ( ToJSON, encode )
import           Data.Row.Records   hiding ( map )
import           Data.Type.Equality

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

instance {-# OVERLAPPABLE #-} ToJSON a => ToParam a where
  toParam a = toStrict $ decodeUtf8 $ encode a

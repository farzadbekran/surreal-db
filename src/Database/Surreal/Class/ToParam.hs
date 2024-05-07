{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Surreal.Class.ToParam where

import           ClassyPrelude

import           Data.Aeson       ( ToJSON, encode )
import           Data.Row.Records hiding ( map )

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

instance {-# OVERLAPPABLE #-} ToParam a => ToParam [a] where
  toParam as = "[" <> intercalate "," (map toParam as) <> "]"

instance {-# OVERLAPPABLE #-} ToJSON a => ToParam a where
  toParam a = toStrict $ decodeUtf8 $ encode a

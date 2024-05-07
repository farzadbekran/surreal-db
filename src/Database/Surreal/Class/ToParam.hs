{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Surreal.Class.ToParam where

import           ClassyPrelude      hiding ( isNothing )

import           Data.Aeson         ( encode )
import           Data.Aeson.Types
import           Data.Row.Records   hiding ( map )
import           Data.Type.Equality
import           GHC.Generics

-- | a class to convert from haskell values to SurrealDB params.
-- we can't use ToJSON because it does not support values like NONE
-- or surrealDB expressions since they are not standard JSON values.
-- there is a default implementation for data types implementing Generic
-- also there is an implementation for data types that have a ToJSON
-- instance. to use the Generic implementation derive Generic and ToParam
class ToParam a where
  toParam :: a -> Text
  default toParam :: (Generic a, GToParam (Rep a)) => a -> Text
  toParam = gtoParam . from

class GToParam f where
  gtoParam :: f p -> Text

class IsRecord f where
  isRecord :: f p -> Bool

instance IsRecord (a :*: b) where
  isRecord _ = True

instance IsRecord (M1 S s (K1 i c)) where
  isRecord _ = True

instance (IsRecord f, GToParam f, Constructor c) => GToParam (M1 C c f) where
  gtoParam m@(M1 x) = if isRecord x
    then "{" <> gtoParam x <> "}"
    else pack ("\"" <> conName m <> "\"")

instance {-# OVERLAPPING #-} (Constructor c) => GToParam (M1 C c U1) where
  gtoParam m = pack ("\"" <> conName m <> "\"")

instance (Selector s, GToParam a) => GToParam (M1 S s a) where
  gtoParam m@(M1 x) = "\"" <> pack (selName m) <> "\": " <> gtoParam x

instance (GToParam a, GToParam b) => GToParam (a :*: b) where
  gtoParam (a :*: b) = gtoParam a <> ", " <> gtoParam b

instance (GToParam a, GToParam b) => GToParam (a :+: b) where
  gtoParam (L1 x) = gtoParam x
  gtoParam (R1 x) = gtoParam x

instance (GToParam f) => GToParam (M1 D d f) where
  gtoParam (M1 x) = gtoParam x

instance ToParam c => GToParam (K1 i c) where
  gtoParam (K1 x) = toParam x

instance {-# OVERLAPPING #-} Forall r ToParam => ToParam (Rec r) where
  toParam r = let fieldList = eraseWithLabels @ToParam toParam r in
    "{" <> intercalate ", " (map getField fieldList) <> "}"
    where
      getField :: (String, Text) -> Text
      getField (label, val) = "\"" <> pack label <> "\": " <> val

instance {-# OVERLAPPABLE #-} ToJSON a => ToParam a where
  toParam a = toStrict $ decodeUtf8 $ encode a

instance {-# OVERLAPPING #-} ToParam a => ToParam (Maybe a) where
  toParam (Just a) = toParam a
  toParam Nothing  = "NONE"

-- | for any kind of list like data type (List, Vector, etc)
instance {-# OVERLAPPING #-} (ToParam a, Functor t, MonoFoldable (t Text), Element (t Text) ~ Text) => ToParam (t a) where
  toParam ta = "[" <> intercalate ", " (toList $ fmap toParam ta) <> "]"

-- data Test = Test1 | Test2 | Test3
--   deriving (Generic, ToParam)

-- data Test4
--   = Test4
--       { name :: !(Maybe Text)
--       , age  :: !Int
--       , tt   :: !Test
--       , tt2  :: ![Test]
--       }
--   deriving (Generic, ToParam)

-- tmp :: Rec ("name" .== Maybe Text .+ "age" .== Int)
-- tmp = #name .== Nothing .+ #age .== 123

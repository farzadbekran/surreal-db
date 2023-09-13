{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Surreal.AST where

import           ClassyPrelude
import           Data.Data         ( cast )
import           Data.Decimal
import           Data.Kind         ( Type )
import           Data.Row
import qualified Data.Text         as T
import           Data.Time.ISO8601 ( formatISO8601 )
import           GHC.TypeLits      ( symbolVal )

newtype FNName
  = FNName Text

type NameSpace = Text
type DataBase = Text

data USE
  = USE NameSpace DataBase
  | USE_NS NameSpace
  | USE_DB DataBase

newtype Param a
  = Param Text

data Field l a where
  Field :: forall a l. (KnownSymbol l) => Label l -> Field l a

data Selector l (r :: Row Type) where
  FieldSelector :: Field l a -> Selector l (l .== a)
  ExpSelector :: (KnownSymbol l) => Exp e -> Label l -> Selector l (l .== e)
  FieldSelectorAs :: (KnownSymbol l') => Field l r -> Label l' -> Selector l (l' .== e)

instance ToQL (Selector l r) where
  toQL = \case
    FieldSelector (Field l) -> prepText [pack $ symbolVal l]
    ExpSelector e l ->
      prepText [ toQL e
               , "AS"
               , pack $ symbolVal l]
    FieldSelectorAs (Field l) l' ->
      prepText [ pack $ symbolVal l
               , "AS"
               , pack $ symbolVal l']

data Fields (r :: Row Type) where
  EmptyFields :: Fields Empty
  (:|) :: Field l a -> Fields r -> Fields (l .== a .+ r)

data Selectors (r :: Row Type) where
  EmptySelectors :: Selectors Empty
  (:||) :: Selector l r1 -> Selectors r2 -> Selectors (r1 .+ r2)

instance ToQL (Selectors r) where
  toQL = \case
    EmptySelectors -> ""
    s :|| ss ->
      toQL s
      <> case toQL ss of
           "" -> ""
           t  -> "," <> t

data VALUE = VALUE

data OMIT (a :: Row Type) where
  OMIT :: Selectors r -> OMIT r

instance ToQL (OMIT r) where
  toQL (OMIT sels) = toQL sels

data ONLY = ONLY

type IndexName = Text -- do we need to improve this?

instance ToQL Text where
  toQL = id

data INDEX
  = NOINDEX
  | INDEX [IndexName]

instance ToQL INDEX where
  toQL NOINDEX    = "NOINDEX"
  toQL (INDEX is) = prepText $ intersperse "," $ map toQL is

newtype WITH
  = WITH INDEX

instance ToQL WITH where
  toQL (WITH index) = toQL index

data FROM where
  FROM :: (Maybe ONLY) -> (Exp a) -> (Maybe WITH) -> FROM

instance ToQL FROM where
  toQL (FROM mOnly exp mWith) =
    prepText [ "FROM"
             , if isJust mOnly then "ONLY" else ""
             , toQL exp
             , renderIfJust mWith
             ]

newtype WHERE
  = WHERE (Exp Bool)

instance ToQL WHERE where
  toQL (WHERE e) = prepText ["WHERE", toQL e]

data SPLIT where
  SPLIT :: Selectors r -> SPLIT

data GROUP where
  GROUP :: Selectors r -> GROUP

data OrderType = RAND | COLLATE | NUMERIC

data OrderDirection = ASC | DESC

data ORDER where
  ORDER :: [(Field l a, Maybe OrderType, Maybe OrderDirection)] -> ORDER

newtype LIMIT
  = LIMIT Int64

newtype START
  = START Int64

data FETCH where
  FETCH :: Selectors r -> FETCH

newtype TIMEOUT
  = TIMEOUT Int64

data PARALLEL = PARALLEL

data EXPLAIN = EXPLAIN | EXPLAINFULL

newtype Duration
  = Duration Text
  -- ^ need to improve this to support formats like '1h3m13s' etc

newtype TableName
  = TableName Text

data Object (a :: Row Type) where
  (:=) :: Label l -> Exp a -> Object (l .== a)
  (:+) :: Object a -> Object b -> Object (a .+ b)

newtype Array a
  = Array [Exp a]

data RecordID a where
  RecordID :: TableName -> ID a -> RecordID a

data ID a where
  TextID :: Text -> ID Text
  NumID :: Int64 -> ID Int64
  ObjID :: Object a -> ID (Object a)
  ArrID :: Array a -> ID [a]

data Literal a where
  NoneL :: Literal ()
  NullL :: Literal ()
  BoolL :: Bool -> Literal Bool
  TextL :: Text -> Literal Text
  Int64L :: Int64 -> Literal Int64
  DecimalL :: Decimal -> Literal Decimal
  FloatL :: Float -> Literal Float
  DateTimeL :: UTCTime -> Literal UTCTime
  DurationL :: Duration -> Literal Duration
  ObjectL :: Object a -> Literal (Object a)
  ArrayL :: Array a -> Literal [a]
  RecordIDL :: RecordID a -> Literal (RecordID a)
  FutureL :: Exp a -> Literal a

data SomeExpression
  = forall e. (Expression e, ToQL e) => SomeExpression e

instance ToQL SomeExpression where
  toQL (SomeExpression e) = toQL e

class (Typeable e, ToQL e) => Expression e where
  toExpression :: e -> SomeExpression
  toExpression = SomeExpression
  fromExpression :: SomeExpression -> Maybe e
  fromExpression (SomeExpression e) = cast e

data Exp (a :: Type) where
  OPE :: FNName -> Exp a -> Exp b -> Exp c
  AppE :: FNName -> [SomeExpression] -> Exp a
  LitE :: Literal a -> Exp a
  ConstE :: Text -> Exp a
  ParamE :: Param a -> Exp a
  IfThenE :: Exp Bool -> Exp a -> Exp a
  IfThenElseE :: Exp Bool -> Exp a -> Exp a -> Exp a
  SelectE :: (Maybe VALUE) -> Selectors r -> (Maybe (OMIT r)) -> FROM -> (Maybe WHERE) -> (Maybe SPLIT) -> (Maybe GROUP) -> (Maybe ORDER) -> (Maybe LIMIT) -> (Maybe START) -> (Maybe FETCH) -> (Maybe TIMEOUT) -> (Maybe PARALLEL) -> Maybe EXPLAIN -> Exp (Var r)

deriving instance Typeable (Exp e)
instance Typeable e => Expression (Exp e)

data Statement where
  UseS :: USE -> Statement
  LetS :: Param a -> Exp a -> Statement
  BeginS :: Statement
  CancelS :: Statement
  CommitS :: Statement
  BreakS :: Statement
  ContinueS :: Statement
  ForS :: Param a -> Exp a -> Block -> Statement

data SurQLLine where
  ExpLine :: Exp a -> SurQLLine
  StatementLine :: Statement -> SurQLLine

data Block where
  Block :: [SurQLLine] -> Block

prepText :: [Text] -> Text
prepText ts = T.replace "  " " " ((unwords . map T.strip) ts)

class ToQL a where
  toQL :: a -> Text

instance ToQL (Literal a) where
  toQL = \case
    NoneL -> "NONE"
    NullL -> "Null"
    BoolL b -> if b then "true" else "false"
    TextL t -> t
    Int64L i64 -> (pack . show) i64
    DecimalL d -> (pack . show) d
    FloatL f -> (pack . show) f
    DateTimeL dt -> pack $ formatISO8601 dt
    DurationL (Duration d) -> d
    _ -> "unimplemented!"

renderIfJust :: ToQL p => Maybe p -> Text
renderIfJust = maybe "" toQL

instance ToQL (Exp a) where
  toQL = \case
    OPE (FNName fnName) e1 e2 -> prepText [toQL e1, fnName, toQL e2]
    AppE (FNName fnName) ps -> prepText $ [fnName <> "("] <> intersperse ", " (map toQL ps) <> [")"]
    LitE le -> toQL le
    ConstE t -> t
    ParamE (Param p) -> "$" <> p
    IfThenE e te -> prepText [ "IF", "(", toQL e, ")"
                             , "THEN", toQL te
                             , "END"]
    IfThenElseE e te fe -> prepText [ "IF", "(", toQL e, ")"
                                    , "THEN", toQL te
                                    , "ELSE", toQL fe
                                    , "END"]
    SelectE mValue selectors mOmit from mWhere mSplit mGroup mOrder mLimit mStart mFetch mTimeout mParallel mExplain ->
      let
      in
      prepText [ "SELECT"
               , if isJust mValue then "VALUE" else ""
               , toQL selectors
               , renderIfJust mOmit
               , toQL from
               , renderIfJust mWhere
               ]
    _ -> error "unimplemented!"

test :: Exp (Var ("age2" .== Int64))
test = SelectE Nothing
  (FieldSelectorAs (Field @Int64 #age) #age2 :|| EmptySelectors)
  Nothing
  (FROM Nothing (ConstE "users") Nothing)
  (Just (WHERE (OPE (FNName ">") (ConstE "age") (LitE (Int64L 18)) :: Exp Bool)))
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

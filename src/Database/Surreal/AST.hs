{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Surreal.AST where

import           ClassyPrelude
import qualified Data.Text         as T
import           Data.Time.ISO8601 ( formatISO8601 )

newtype FNName
  = FNName Text
  deriving (Eq, Generic, Read, Show)

data Operator
  = (:&&)
  | (:||)
  | (:??)
  | (:?:)
  | (:=)
  | (:!=)
  | (:==)
  | (:?=)
  | (:*=)
  | (:~)
  | (:!~)
  | (:?~)
  | (:*~)
  | (:<)
  | (:<=)
  | (:>)
  | (:>=)
  | (:+)
  | (:-)
  | (:*)
  | (:/)
  | (:**)
  | IN
  | NOTIN
  | CONTAINS
  | CONTAINSNOT
  | CONTAINSALL
  | CONTAINSANY
  | CONTAINSNONE
  | INSIDE
  | NOTINSIDE
  | ALLINSIDE
  | ANYINSIDE
  | NONEINSIDE
  | OUTSIDE
  | INTERSECTS
  | (:@@)
  deriving (Eq, Generic, Read, Show)

instance ToQL Operator where
  toQL = \case
    (:&&) -> "&&"
    (:||) -> "||"
    (:??) -> "??"
    (:?:) -> "?:"
    (:=) -> "="
    (:!=) -> "!="
    (:==) -> "=="
    (:?=) -> "?="
    (:*=) -> "*="
    (:~) -> ":~"
    (:!~) -> "!~"
    (:?~) -> "?~"
    (:*~) -> "*~"
    (:<) -> "<"
    (:<=) -> "<="
    (:>) -> ">"
    (:>=) -> ">="
    (:+) -> "+"
    (:-) -> "-"
    (:*) -> "*"
    (:/) -> "/"
    (:**) -> "**"
    IN -> "IN"
    NOTIN -> "NOTIN"
    CONTAINS -> "CONTAINS"
    CONTAINSNOT -> "CONTAINSNOT"
    CONTAINSALL -> "CONTAINSALL"
    CONTAINSANY -> "CONTAINSANY"
    CONTAINSNONE -> "CONTAINSNONE"
    INSIDE -> "INSIDE"
    NOTINSIDE -> "NOTINSIDE"
    ALLINSIDE -> "ALLINSIDE"
    ANYINSIDE -> "ANYINSIDE"
    NONEINSIDE -> "NONEINSIDE"
    OUTSIDE -> "OUTSIDE"
    INTERSECTS -> "INTERSECTS"
    (:@@) -> "@@"

type Namespace = Text
type Database = Text

data TypeDef = T String [ TypeDef ]
  deriving (Eq, Generic, Read, Show)

data USE
  = USE Namespace Database
  | USE_NS Namespace
  | USE_DB Database
  deriving (Eq, Generic, Read, Show)

newtype Param
  = Param Text
  deriving (Eq, Generic, Read, Show)

data Field
  = SimpleField Text -- ^ name
  | IndexedField Field [Literal] -- ^ address[0]
  | FilteredField Field WHERE -- ^ (address WHERE city = "New York")
  | CompositeField Field Field -- ^ address.city
  deriving (Eq, Generic, Read, Show)

instance ToQL Field where
  toQL (SimpleField t) = t
  toQL (IndexedField t is) = prepText $ [toQL t] <> concatMap (\i -> ["[", toQL i, "]"]) is
  toQL (FilteredField f w) = prepText ["(", toQL f, toQL w, ")"]
  toQL (CompositeField f1 f2) = prepText [toQL f1, ".", toQL f2]

data Edge
  = OutEdge Field -- ^ ->bought
  | InEdge Field -- ^ <-bought
  deriving (Eq, Generic, Read, Show)

instance ToQL Edge where
  toQL (OutEdge f) = prepText [ "->", toQL f ]
  toQL (InEdge f) = prepText [ "<-", toQL f ]

-- TODO: add edge selectors like ->user->likes-> etc
data Selector
  = FieldSelector Field
  | ExpSelector Exp Field
  | FieldSelectorAs Field Field
  | EdgeSelector (Maybe Field) [Edge] -- ^ f1->f2<-f3 or ->f2<-f3
  | TypedSelector Selector TypeDef
  deriving (Eq, Generic, Read, Show)

instance ToQL Selector where
  toQL = \case
    FieldSelector f -> prepText [toQL f] -- TODO: remove this to prevent untyped selectors?
    ExpSelector e f ->
      prepText [ toQL e, "AS", toQL f ]
    FieldSelectorAs f fAs ->
      prepText [ toQL f, "AS", toQL fAs ]
    EdgeSelector mf es ->
      prepText $ maybe "" toQL mf : map toQL es
    TypedSelector s _ -> toQL s

newtype Selectors
  = Selectors [Selector]
  deriving (Eq, Generic, Read, Show)

instance ToQL Selectors where
  toQL (Selectors []) = ""
  toQL (Selectors ss) = prepText $ intersperse "," $ map toQL ss

data VALUE = VALUE
  deriving (Eq, Generic, Read, Show)

instance ToQL VALUE where
  toQL VALUE = "VALUE"

newtype OMIT
  = OMIT [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL OMIT where
  toQL (OMIT []) = ""
  toQL (OMIT fs) = prepText $ ["OMIT"] <> intersperse "," (map toQL fs)

data ONLY = ONLY
  deriving (Eq, Generic, Read, Show)

type IndexName = Text -- do we need to improve this?

instance ToQL Text where
  toQL = id

data INDEX
  = NOINDEX
  | INDEX [IndexName]
  deriving (Eq, Generic, Read, Show)

instance ToQL INDEX where
  toQL NOINDEX    = "NOINDEX"
  toQL (INDEX is) = prepText $ intersperse "," $ map toQL is

newtype WITH
  = WITH INDEX
  deriving (Eq, Generic, Read, Show)

instance ToQL WITH where
  toQL (WITH i) = toQL i

data FROM
  = FROM (Maybe ONLY) Exp (Maybe WITH)
  deriving (Eq, Generic, Read, Show)

instance ToQL FROM where
  toQL (FROM mOnly e mWith) =
    prepText [ "FROM"
             , if isJust mOnly then "ONLY" else ""
             , toQL e
             , renderIfJust mWith
             ]

newtype WHERE
  = WHERE Exp
  deriving (Eq, Generic, Read, Show)

instance ToQL WHERE where
  toQL (WHERE e) = prepText ["WHERE", toQL e]

newtype SPLIT
  = SPLIT [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL SPLIT where
  toQL (SPLIT []) = ""
  toQL (SPLIT fs) = prepText $ ["SPLIT"] <> intersperse "," (map toQL fs)

newtype GROUP
  = GROUP [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL GROUP where
  toQL (GROUP []) = ""
  toQL (GROUP fs) = prepText $ ["GROUP BY"] <> intersperse "," (map toQL fs)

data OrderType = RAND | COLLATE | NUMERIC
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderType where
  toQL = \case
    RAND -> "RAND"
    COLLATE -> "COLLATE"
    NUMERIC -> "NUMERIC"

data OrderDirection = ASC | DESC
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderDirection where
  toQL = \case
    ASC -> "ASC"
    DESC -> "DESC"

newtype ORDER
  = ORDER [(Field, Maybe OrderType, Maybe OrderDirection)]
  deriving (Eq, Generic, Read, Show)

instance ToQL ORDER where
  toQL (ORDER []) = ""
  toQL (ORDER fs) = prepText $ ["ORDER BY"] <> intersperse "," (map renderOrder fs)
    where
      renderOrder (f, mOrderType, mOrderDirection) =
        prepText [ toQL f, renderIfJust mOrderType, renderIfJust mOrderDirection ]

newtype LIMIT
  = LIMIT Int64
  deriving (Eq, Generic, Read, Show)

instance ToQL LIMIT where
  toQL (LIMIT i) = "LIMIT " <> tshow i

newtype START
  = START Int64
  deriving (Eq, Generic, Read, Show)

instance ToQL START where
  toQL (START i) = "START " <> tshow i

newtype FETCH
  = FETCH [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL FETCH where
  toQL (FETCH []) = ""
  toQL (FETCH fs) = prepText $ ["FETCH"] <> intersperse "," (map toQL fs)

newtype TIMEOUT
  = TIMEOUT Int64
  deriving (Eq, Generic, Read, Show)

instance ToQL TIMEOUT where
  toQL (TIMEOUT i) = "TIMEOUT " <> tshow i

data PARALLEL = PARALLEL
  deriving (Eq, Generic, Read, Show)

instance ToQL PARALLEL where
  toQL PARALLEL = "PARALLEL"

data EXPLAIN = EXPLAIN | EXPLAINFULL
  deriving (Eq, Generic, Read, Show)

instance ToQL EXPLAIN where
  toQL EXPLAIN = "EXPLAIN"
  toQL EXPLAINFULL = "EXPLAINFULL"

-- | duration formats like "1y2w3d", seuureal db currently does not support months
data Duration
  = Duration { _y :: Int64
             , _w :: Int64
             , _d :: Int64
             , _h :: Int64
             , _m :: Int64
             , _s :: Int64
             , _ms :: Int64
             , _us :: Int64
             , _ns :: Int64
             }
  deriving (Eq, Generic, Read, Show)

defaultDuration :: Duration
defaultDuration = Duration 0 0 0 0 0 0 0 0 0

newtype TableName
  = TableName Text
  deriving (Eq, Generic, Read, Show)

newtype Object
  = Object [(Field, Exp)]
  deriving (Eq, Generic, Read, Show)

data RecordID
  = RecordID TableName ID
  deriving (Eq, Generic, Read, Show)

data ID
  = TextID Text
  | NumID Int64
  | ObjID Object
  | TupID [Exp]
  deriving (Eq, Generic, Read, Show)

data Literal
  = NoneL
  | NullL
  | BoolL Bool
  | TextL Text
  | Int64L Int64
  | FloatL Float
  | DateTimeL UTCTime
  | DurationL Duration
  | ObjectL Object
  | ArrayL [Exp]
  | RecordIDL RecordID
  | FutureL Exp
  deriving (Eq, Generic, Read, Show)

-- TODO: add typed input
data Exp
  = TypedE Exp TypeDef
  | OPE Operator Exp Exp
  | AppE FNName [Exp]
  | LitE Literal
  | ConstE Text
  | ParamE Param
  | IfThenE Exp Exp
  | IfThenElseE Exp Exp Exp
  | SelectE (Maybe VALUE) Selectors (Maybe OMIT) FROM (Maybe WHERE) (Maybe SPLIT) (Maybe GROUP) (Maybe ORDER) (Maybe LIMIT) (Maybe START) (Maybe FETCH) (Maybe TIMEOUT) (Maybe PARALLEL) (Maybe EXPLAIN)
  deriving (Eq, Generic, Read, Show)

data Statement
  = UseS USE
  | LetS Param Exp
  | BeginS
  | CancelS
  | CommitS
  | BreakS
  | ContinueS
  | ForS Param Exp Block
  deriving (Eq, Generic, Read, Show)

data SurQLLine
  = ExpLine Exp
  | StatementLine Statement
  deriving (Eq, Generic, Read, Show)

newtype Block
  = Block [SurQLLine]
  deriving (Eq, Generic, Read, Show)

prepText :: [Text] -> Text
prepText ts = T.replace "  " " " ((unwords . map T.strip) ts)

class ToQL a where
  toQL :: a -> Text

instance ToQL Literal where
  toQL = \case
    NoneL -> "NONE"
    NullL -> "Null"
    BoolL b -> if b then "true" else "false"
    TextL t -> t
    Int64L i64 -> (pack . show) i64
    FloatL f -> (pack . show) f
    DateTimeL dt -> pack $ formatISO8601 dt
    DurationL (Duration { .. }) ->
      let frmt i l = if i > 0 then (pack . show) i <> l else "" in
      frmt _y "y"
      <> frmt _w "w"
      <> frmt _d "d"
      <> frmt _h "h"
      <> frmt _m "m"
      <> frmt _s "s"
      <> frmt _ms "ms"
      <> frmt _us "us"
      <> frmt _ns "ns"
    _ -> "unimplemented!"

renderIfJust :: ToQL p => Maybe p -> Text
renderIfJust = maybe "" toQL

instance ToQL Exp where
  toQL = \case
    TypedE e _ -> toQL e
    OPE op e1 e2 -> prepText [toQL e1, toQL op, toQL e2]
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
      prepText [ "SELECT"
               , renderIfJust mValue
               , toQL selectors
               , renderIfJust mOmit
               , toQL from
               , renderIfJust mWhere
               , renderIfJust mSplit
               , renderIfJust mGroup
               , renderIfJust mOrder
               , renderIfJust mLimit
               , renderIfJust mStart
               , renderIfJust mFetch
               , renderIfJust mTimeout
               , renderIfJust mParallel
               , renderIfJust mExplain
               ]
    --_ -> error "expression toQL undefined!"

-- test :: Exp
-- test = SelectE Nothing
--   (Selectors [FieldSelectorAs (Field "age") "age2"])
--   Nothing
--   (FROM Nothing (ConstE "users") Nothing)
--   (Just (WHERE (OPE (FNName ">") (ConstE "age") (LitE (Int64L 18)))))
--   Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

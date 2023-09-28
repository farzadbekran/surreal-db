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

data USE
  = USE Namespace Database
  | USE_NS Namespace
  | USE_DB Database
  deriving (Eq, Generic, Read, Show)

newtype Param
  = Param Text
  deriving (Eq, Generic, Read, Show)

newtype Field
  = Field Text
  deriving (Eq, Generic, Read, Show)

instance ToQL Field where
  toQL (Field t) = t

data Selector
  = FieldSelector Field
  | ExpSelector Exp Field
  | FieldSelectorAs Field Field
  | TypedSelector Selector Text
  deriving (Eq, Generic, Read, Show)

instance ToQL Selector where
  toQL = \case
    FieldSelector (Field f) -> prepText [f] -- TODO: remove this to prevent untyped selectors?
    ExpSelector e (Field f) ->
      prepText [ toQL e, "AS", f ]
    FieldSelectorAs (Field f) (Field fAs) ->
      prepText [ f, "AS", fAs ]
    TypedSelector s _ -> toQL s

newtype Selectors
  = Selectors [Selector]
  deriving (Eq, Generic, Read, Show)

instance ToQL Selectors where
  toQL (Selectors []) = ""
  toQL (Selectors ss) = prepText $ intersperse "," $ map toQL ss

data VALUE = VALUE
  deriving (Eq, Generic, Read, Show)

newtype OMIT
  = OMIT [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL OMIT where
  toQL (OMIT []) = ""
  toQL (OMIT fs) = prepText $ intersperse "," $ map toQL fs

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

newtype GROUP
  = GROUP [Field]
  deriving (Eq, Generic, Read, Show)

data OrderType = RAND | COLLATE | NUMERIC
  deriving (Eq, Generic, Read, Show)

data OrderDirection = ASC | DESC
  deriving (Eq, Generic, Read, Show)

newtype ORDER
  = ORDER [(Field, Maybe OrderType, Maybe OrderDirection)]
  deriving (Eq, Generic, Read, Show)

newtype LIMIT
  = LIMIT Int64
  deriving (Eq, Generic, Read, Show)

newtype START
  = START Int64
  deriving (Eq, Generic, Read, Show)

newtype FETCH
  = FETCH [Field]
  deriving (Eq, Generic, Read, Show)

newtype TIMEOUT
  = TIMEOUT Int64
  deriving (Eq, Generic, Read, Show)

data PARALLEL = PARALLEL
  deriving (Eq, Generic, Read, Show)

data EXPLAIN = EXPLAIN | EXPLAINFULL
  deriving (Eq, Generic, Read, Show)

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

-- TODO: add edge selectors like ->user->likes-> etc
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

data Exp
  = TypedE Exp Text
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
      let
      in
      prepText [ "SELECT"
               , if isJust mValue then "VALUE" else ""
               , toQL selectors
               , renderIfJust mOmit
               , toQL from
               , renderIfJust mWhere
               ]
    --_ -> error "expression toQL undefined!"

-- test :: Exp
-- test = SelectE Nothing
--   (Selectors [FieldSelectorAs (Field "age") "age2"])
--   Nothing
--   (FROM Nothing (ConstE "users") Nothing)
--   (Just (WHERE (OPE (FNName ">") (ConstE "age") (LitE (Int64L 18)))))
--   Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

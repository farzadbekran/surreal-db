{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Database.Surreal.AST where

import           ClassyPrelude
import           Data.Foldable     ( foldl1 )
import qualified Data.Text         as T
import           Data.Time.ISO8601 ( formatISO8601 )

prepText :: [Text] -> Text
prepText = unwords . filter (/= "") . map T.strip

renderIfJust :: ToQL p => Maybe p -> Text
renderIfJust = maybe "" toQL

class ToQL a where
  toQL :: a -> Text

class HasInput a where
  getInputs :: a -> [Input]

newtype FNName
  = FNName Text
  deriving (Eq, Generic, Read, Show)

data Operator = (:&&) | (:||) | (:??) | (:?:) | (:=) | (:!=) | (:==) | (:?=) | (:*=) | (:~) | (:!~) | (:?~) | (:*~) | (:<) | (:<=) | (:>) | (:>=) | (:+) | (:-) | (:*) | (:/) | (:**) | (:+=) | (:-=) | IN | NOTIN | CONTAINS | CONTAINSNOT | CONTAINSALL | CONTAINSANY | CONTAINSNONE | INSIDE | NOTINSIDE | ALLINSIDE | ANYINSIDE | NONEINSIDE | OUTSIDE | INTERSECTS | (:@@)
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
    (:+=) -> "+="
    (:-=) -> "-="
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

data TypeDef
  = T String [TypeDef]
  deriving (Eq, Generic, Read, Show)

data USE
  = USE Namespace Database
  | USE_NS Namespace
  | USE_DB Database
  deriving (Eq, Generic, Read, Show)

newtype Param
  = Param Text
  deriving (Eq, Generic, Read, Show)

instance ToQL Param where
  toQL (Param t) = "$" <> t

data Field
  = SimpleField Text -- ^ name
  | IndexedField Field [Literal] -- ^ address[0]
  | FilteredField Field WHERE -- ^ (address WHERE city = "New York")
  | CompositeField Field Field -- ^ address.city
  | FieldInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL Field where
  toQL (SimpleField t) = t
  toQL (IndexedField t is) = prepText $ [toQL t] <> concatMap (\i -> ["[", toQL i, "]"]) is
  toQL (FilteredField f w) = prepText ["(", toQL f, toQL w, ")"]
  toQL (CompositeField f1 f2) = foldl1 (<>) [toQL f1, ".", toQL f2]
  toQL (FieldInput i) = toQL i

instance HasInput Field where
  getInputs = \case
    FilteredField _ w -> getInputs w
    FieldInput i -> [i]
    _ -> []

data Edge
  = OutEdge Field -- ^ ->bought
  | InEdge Field -- ^ <-bought
  deriving (Eq, Generic, Read, Show)

instance ToQL Edge where
  toQL (OutEdge f) = "->" <> toQL f
  toQL (InEdge f)  = "<-" <> toQL f

instance HasInput Edge where
  getInputs (OutEdge f) = getInputs f
  getInputs (InEdge f)  = getInputs f

data Selector
  = WildCardSelector
  | FieldSelector Field
  | ExpSelector Exp Field
  | SelectorAs Selector Field
  | EdgeSelector (Maybe Field) [Edge] Field
  -- ^ f1->f2<-f3 or ->f2<-f3, last field is the alias for the results,
  -- we force it since the results are unreliable if the alias is not given!!
  | TypedSelector Selector TypeDef
  deriving (Eq, Generic, Read, Show)

instance ToQL Selector where
  toQL = \case
    WildCardSelector -> "*"
    FieldSelector f -> prepText [toQL f]
    ExpSelector e f ->
      prepText [ toQL e, "AS", toQL f ]
    SelectorAs s fAs ->
      prepText [ toQL s, "AS", toQL fAs ]
    EdgeSelector mf es f ->
      foldl1 (<>) $ maybe "" toQL mf : map toQL es <> [" AS ", toQL f]
    TypedSelector s _ -> toQL s

instance HasInput Selector where
  getInputs = \case
    WildCardSelector -> []
    FieldSelector f -> getInputs f
    ExpSelector e f -> getInputs e <> getInputs f
    SelectorAs s fAs -> getInputs s <> getInputs fAs
    EdgeSelector mf es f -> maybe [] getInputs mf <> concatMap getInputs es <> getInputs f
    TypedSelector s _ -> getInputs s

newtype Selectors
  = Selectors [Selector]
  deriving (Eq, Generic, Read, Show)

instance ToQL Selectors where
  toQL (Selectors []) = ""
  toQL (Selectors ss) = prepText $ intersperse "," $ map toQL ss

instance HasInput Selectors where
  getInputs (Selectors ss) = concatMap getInputs ss

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

instance ToQL ONLY where
  toQL ONLY = "ONLY"

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

instance HasInput FROM where
  getInputs (FROM _ e _) = getInputs e

newtype WHERE
  = WHERE Exp
  deriving (Eq, Generic, Read, Show)

instance ToQL WHERE where
  toQL (WHERE e) = prepText ["WHERE", toQL e]

instance HasInput WHERE where
  getInputs (WHERE e) = getInputs e

newtype SPLIT
  = SPLIT [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL SPLIT where
  toQL (SPLIT []) = ""
  toQL (SPLIT fs) = prepText $ ["SPLIT"] <> intersperse "," (map toQL fs)

instance HasInput SPLIT where
  getInputs (SPLIT fs) = concatMap getInputs fs

newtype GROUP
  = GROUP [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL GROUP where
  toQL (GROUP []) = ""
  toQL (GROUP fs) = prepText $ ["GROUP BY"] <> intersperse "," (map toQL fs)

instance HasInput GROUP where
  getInputs (GROUP fs) = concatMap getInputs fs

data OrderType
  = RAND
  | COLLATE
  | NUMERIC
  | OrderTypeInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderType where
  toQL = \case
    RAND -> "RAND"
    COLLATE -> "COLLATE"
    NUMERIC -> "NUMERIC"
    OrderTypeInput i -> toQL i

instance HasInput OrderType where
  getInputs (OrderTypeInput i) = [i]
  getInputs _                  = []

data OrderDirection
  = ASC
  | DESC
  | OrderDirectionInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderDirection where
  toQL = \case
    ASC -> "ASC"
    DESC -> "DESC"
    OrderDirectionInput i -> toQL i

instance HasInput OrderDirection where
  getInputs (OrderDirectionInput i) = [i]
  getInputs _                       = []

newtype ORDER
  = ORDER [(Field, Maybe OrderType, Maybe OrderDirection)]
  deriving (Eq, Generic, Read, Show)

instance ToQL ORDER where
  toQL (ORDER []) = ""
  toQL (ORDER fs) = prepText $ ["ORDER BY"] <> intersperse "," (map renderOrder fs)
    where
      renderOrder (f, mOrderType, mOrderDirection) =
        prepText [ toQL f, renderIfJust mOrderType, renderIfJust mOrderDirection ]

instance HasInput ORDER where
  getInputs (ORDER os) =
    concatMap (\(f, mOtype, mOdirection)
               -> getInputs f
                <> maybe [] getInputs mOtype
                <> maybe [] getInputs mOdirection)
    os

data LIMIT
  = LIMIT Int64
  | LIMITInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL LIMIT where
  toQL (LIMIT i)      = "LIMIT " <> tshow i
  toQL (LIMITInput i) = "LIMIT " <> toQL i

instance HasInput LIMIT where
  getInputs (LIMITInput i) = [i]
  getInputs _              = []

data START
  = START Int64
  | STARTInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL START where
  toQL (START i)      = "START " <> tshow i
  toQL (STARTInput i) = "START " <> toQL i

instance HasInput START where
  getInputs (STARTInput i) = [i]
  getInputs _              = []

newtype FETCH
  = FETCH [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL FETCH where
  toQL (FETCH []) = ""
  toQL (FETCH fs) = prepText $ ["FETCH"] <> intersperse "," (map toQL fs)

instance HasInput FETCH where
  getInputs (FETCH fs) = concatMap getInputs fs

data TIMEOUT
  = TIMEOUT Duration
  | TIMEOUTInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL TIMEOUT where
  toQL (TIMEOUT d)      = "TIMEOUT " <> toQL d
  toQL (TIMEOUTInput i) = "TIMEOUT " <> toQL i

data PARALLEL = PARALLEL
  deriving (Eq, Generic, Read, Show)

instance ToQL PARALLEL where
  toQL PARALLEL = "PARALLEL"

data EXPLAIN = EXPLAIN | EXPLAINFULL
  deriving (Eq, Generic, Read, Show)

instance ToQL EXPLAIN where
  toQL EXPLAIN     = "EXPLAIN"
  toQL EXPLAINFULL = "EXPLAINFULL"

-- | duration formats like "1y2w3d", surreal db currently does not support months
data Duration
  = Duration
      { _y  :: Int64
      , _w  :: Int64
      , _d  :: Int64
      , _h  :: Int64
      , _m  :: Int64
      , _s  :: Int64
      , _ms :: Int64
      , _us :: Int64
      , _ns :: Int64
      }
  deriving (Eq, Generic, Read, Show)

instance ToQL Duration where
  toQL Duration { _y
                , _w
                , _d
                , _h
                , _m
                , _s
                , _ms
                , _us
                , _ns
                }
    = renderIfNonZero _y "y"
    <> renderIfNonZero _w "w"
    <> renderIfNonZero _d "d"
    <> renderIfNonZero _h "h"
    <> renderIfNonZero _m "m"
    <> renderIfNonZero _s "s"
    <> renderIfNonZero _ms "ms"
    <> renderIfNonZero _us "us"
    <> renderIfNonZero _ns "ns"
    where
      renderIfNonZero i unit = if i /= 0 then tshow i <> unit else ""

defaultDuration :: Duration
defaultDuration = Duration 0 0 0 0 0 0 0 0 0

data TableName
  = TableName Text
  | TableNameInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL TableName where
  toQL (TableName t)      = t
  toQL (TableNameInput i) = toQL i

instance HasInput TableName where
  getInputs (TableNameInput i) = [i]
  getInputs _                  = []

newtype Object
  = Object [(Field, Exp)]
  deriving (Eq, Generic, Read, Show)

instance ToQL Object where
  toQL (Object fs) = prepText $ "{" : intersperse "," (map renderField fs) <> ["}"]
    where
      renderField :: (Field, Exp) -> Text
      renderField (f, e) = toQL f <> ": " <> toQL e

instance HasInput Object where
  getInputs (Object fs)
    = concatMap (\(f, e) -> getInputs f <> getInputs e)
      fs

data RecordID
  = RecordID TableName ID
  | RecordIDInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL RecordID where
  toQL (RecordID t i)    = toQL t <> ":" <> toQL i
  toQL (RecordIDInput i) = toQL i

instance HasInput RecordID where
  getInputs (RecordIDInput i) = [i]
  getInputs (RecordID t i)    = getInputs t <> getInputs i

data RandFNName = RNUUID | RNRAND | RNULID
  deriving (Eq, Generic, Read, Show)

instance ToQL RandFNName where
  toQL = \case
    RNUUID -> "uuid()"
    RNRAND -> "rand()"
    RNULID -> "ulid()"

data ID
  = TextID Text
  | NumID Int64
  | ObjID Object
  | TupID [Exp]
  | RandomID RandFNName
  | IDInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL ID where
  toQL = \case
    TextID t -> "`" <> t <> "`"
    NumID i -> tshow i
    ObjID o -> toQL o
    TupID es -> prepText $ ["["] <> intersperse "," (map toQL es) <> ["]"]
    RandomID fn -> toQL fn
    IDInput i -> toQL i

instance HasInput ID where
  getInputs (ObjID o)   = getInputs o
  getInputs (TupID es)  = concatMap getInputs es
  getInputs (IDInput i) = [i]
  getInputs _           = []

data IDRange
  = IDRangeGT ID
  | IDRangeLT ID
  | IDRangeBetween ID ID
  deriving (Eq, Generic, Read, Show)

instance ToQL IDRange where
  toQL (IDRangeLT i)          = ".." <> toQL i
  toQL (IDRangeGT i)          = toQL i <> ".."
  toQL (IDRangeBetween i1 i2) = toQL i1 <> ".." <> toQL i2

instance HasInput IDRange where
  getInputs (IDRangeGT i)          = getInputs i
  getInputs (IDRangeLT i)          = getInputs i
  getInputs (IDRangeBetween i1 i2) = getInputs i1 <> getInputs i2

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
  | RecordIDRangeL TableName IDRange
  | FutureL Exp
  | LiteralInput Input
  deriving (Eq, Generic, Read, Show)

instance ToQL Literal where
  toQL = \case
    NoneL -> "NONE"
    NullL -> "Null"
    BoolL b -> if b then "true" else "false"
    TextL t -> "'" <> t <> "'"
    Int64L i64 -> (pack . show) i64
    FloatL f -> (pack . show) f
    DateTimeL dt -> pack $ formatISO8601 dt
    DurationL (Duration { .. }) ->
      let frmt i l = if i > 0 then tshow i <> l else "" in
      frmt _y "y"
      <> frmt _w "w"
      <> frmt _d "d"
      <> frmt _h "h"
      <> frmt _m "m"
      <> frmt _s "s"
      <> frmt _ms "ms"
      <> frmt _us "us"
      <> frmt _ns "ns"
    RecordIDRangeL t range -> toQL t <> ":" <> toQL range
    LiteralInput i -> toQL i
    ArrayL es -> "[" <> foldl1 (<>) (intersperse "," (map toQL es)) <> "]"
    ObjectL o -> toQL o
    RecordIDL i -> toQL i
    FutureL e -> "<future> {" <> toQL e <> "}"

instance HasInput Literal where
  getInputs = \case
    ObjectL o -> getInputs o
    ArrayL es -> concatMap getInputs es
    RecordIDL i -> getInputs i
    FutureL e -> getInputs e
    RecordIDRangeL t range -> getInputs t <> getInputs range
    LiteralInput i -> [i]
    _ -> []

data Input
  = Input Int64 TypeDef
  deriving (Eq, Generic, Read, Show)

instance ToQL Input where
  toQL (Input i _) = "%" <> tshow i

data IGNORE = IGNORE
  deriving (Eq, Generic, Read, Show)

instance ToQL IGNORE where
  toQL IGNORE = "IGNORE"

newtype OnDuplicate
  = OnDuplicate [Exp]
  deriving (Eq, Generic, Read, Show)

instance ToQL OnDuplicate where
  toQL (OnDuplicate es) = foldl1 (<>) $ intersperse "," $ map toQL es

instance HasInput OnDuplicate where
  getInputs (OnDuplicate es) = concatMap getInputs es

data InsertVal
  = InsertObjects [Object]
  | InsertValues [Field] [[Exp]] (Maybe OnDuplicate)
  deriving (Eq, Generic, Read, Show)

instance ToQL InsertVal where
  toQL (InsertObjects os) = "[" <> foldl1 (<>) (intersperse "," $ map toQL os) <> "]"
  toQL (InsertValues fs es od)
    = "(" <> foldl1 (<>) (intersperse "," $ map toQL fs) <> ")"
    <> " VALUES "
    <> foldl1 (<>) (intersperse "," $ map renderTuple es)
    <> case od of
         Just (OnDuplicate es')
           -> " ON DUPLICATE KEY UPDATE "
           <> foldl1 (<>) (intersperse "," $ map toQL es')
         Nothing -> ""
    where
      renderTuple :: [Exp] -> Text
      renderTuple exps = "(" <> foldl1 (<>) (intersperse "," $ map toQL exps) <> ")"

instance HasInput InsertVal where
  getInputs (InsertObjects os) = concatMap getInputs os
  getInputs (InsertValues fs es od)
    = concatMap getInputs fs
    <> concatMap (concatMap getInputs) es
    <> maybe [] getInputs od

data Target
  = TargetTable TableName
  | TargetRecID RecordID
  | TargetEdge RecordID [Edge]
  deriving (Eq, Generic, Read, Show)

instance ToQL Target where
  toQL (TargetTable tn)  = toQL tn
  toQL (TargetRecID rid) = toQL rid
  toQL (TargetEdge rid es) =
    foldl1 (<>) $ toQL rid : map toQL es

instance HasInput Target where
  getInputs = \case
    TargetTable tn -> getInputs tn
    TargetRecID rid -> getInputs rid
    TargetEdge rid es -> getInputs rid <> concatMap getInputs es

data CreateVal
  = CreateObject Object
  | CreateValues [(Field, Exp)]
  deriving (Eq, Generic, Read, Show)

instance ToQL CreateVal where
  toQL (CreateObject o)  = "CONTENT " <> toQL o
  toQL (CreateValues fields) = prepText $ "SET" : intersperse "," (map renderTuple fields)
    where
      renderTuple (f,v) = toQL f <> " = " <> toQL v

instance HasInput CreateVal where
  getInputs = \case
    CreateObject o -> getInputs o
    CreateValues fields -> concatMap getTupleInputs fields
    where
      getTupleInputs (f,v) = getInputs f <> getInputs v

data UpdateVal
  = UpdateObject Object
  | UpdateValues [(Field, Exp)]
  | UpdateMerge Object
  | UpdatePatch Object
  deriving (Eq, Generic, Read, Show)

instance ToQL UpdateVal where
  toQL (UpdateObject o)  = "CONTENT " <> toQL o
  toQL (UpdateMerge o)  = "MERGE " <> toQL o
  toQL (UpdatePatch o)  = "Patch " <> toQL o
  toQL (UpdateValues fields) = prepText $ "SET" : intersperse "," (map renderTuple fields)
    where
      renderTuple (f,v) = toQL f <> " = " <> toQL v

instance HasInput UpdateVal where
  getInputs = \case
    UpdateObject o -> getInputs o
    UpdateMerge o -> getInputs o
    UpdatePatch o -> getInputs o
    UpdateValues fields -> concatMap getTupleInputs fields
    where
      getTupleInputs (f,v) = getInputs f <> getInputs v

data ReturnType
  = RTNone
  | RTBefore
  | RTAfter
  | RTDiff
  | RTProjections Selectors
  deriving (Eq, Generic, Read, Show)

instance ToQL ReturnType where
  toQL = \case
    RTNone -> "RETURN NONE"
    RTBefore -> "RETURN BEFORE"
    RTAfter -> "RETURN AFTER"
    RTDiff -> "RETURN DIFF"
    RTProjections (Selectors ss) -> "RETURN " <> prepText (intersperse "," $ map toQL ss)

data Exp
  = TypedE Exp TypeDef
  | OPE Operator Exp Exp
  | AppE FNName [Exp]
  | LitE Literal
  | ConstE Text
  | ParamE Param
  | InputE Input
  | IfThenE Exp Exp
  | IfThenElseE Exp Exp Exp
  | EdgeSelectorE (Maybe Field) [Edge]
  | SelectE (Maybe VALUE) Selectors (Maybe OMIT) FROM (Maybe WHERE) (Maybe SPLIT) (Maybe GROUP) (Maybe ORDER) (Maybe LIMIT) (Maybe START) (Maybe FETCH) (Maybe TIMEOUT) (Maybe PARALLEL) (Maybe EXPLAIN)
  | InsertE (Maybe IGNORE) TableName InsertVal
  | CreateE (Maybe ONLY) Target CreateVal (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | DeleteE (Maybe ONLY) Target (Maybe WHERE) (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | UpdateE (Maybe ONLY) Target UpdateVal (Maybe WHERE) (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  deriving (Eq, Generic, Read, Show)

instance ToQL Exp where
  toQL = \case
    TypedE e _ -> toQL e
    OPE op e1 e2 -> prepText [toQL e1, toQL op, toQL e2]
    AppE (FNName fnName) ps -> prepText $ [fnName <> "("] <> intersperse ", " (map toQL ps) <> [")"]
    LitE le -> toQL le
    ConstE t -> t
    ParamE (Param p) -> "$" <> p
    InputE i -> toQL i
    IfThenE e te -> prepText [ "IF", "(", toQL e, ")"
                             , "THEN", toQL te
                             , "END"]
    IfThenElseE e te fe -> prepText [ "IF", "(", toQL e, ")"
                                    , "THEN", toQL te
                                    , "ELSE", toQL fe
                                    , "END"]
    EdgeSelectorE mf es -> foldl1 (<>) $ maybe "" toQL mf : map toQL es
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
    InsertE mIgnore tableName insertVal ->
      prepText [ "INSERT"
               , renderIfJust mIgnore
               , "INTO"
               , toQL tableName
               , toQL insertVal
               ]
    CreateE mOnly target v mReturn mTimeout mParallel ->
      prepText [ "CREATE"
               , renderIfJust mOnly
               , toQL target
               , toQL v
               , renderIfJust mReturn
               , renderIfJust mTimeout
               , renderIfJust mParallel
               ]
    UpdateE mOnly target v mWhere mReturn mTimeout mParallel ->
      prepText [ "UPDATE"
               , renderIfJust mOnly
               , toQL target
               , toQL v
               , renderIfJust mWhere
               , renderIfJust mReturn
               , renderIfJust mTimeout
               , renderIfJust mParallel
               ]
    DeleteE mOnly target mWhere mReturn mTimeout mParallel ->
      prepText [ "DELETE"
               , renderIfJust mOnly
               , toQL target
               , renderIfJust mWhere
               , renderIfJust mReturn
               , renderIfJust mTimeout
               , renderIfJust mParallel
               ]

instance HasInput Exp where
  getInputs = \case
    TypedE e _ -> getInputs e
    OPE _ e1 e2 -> getInputs e1 <> getInputs e2
    AppE _ ps -> concatMap getInputs ps
    LitE le -> getInputs le
    ParamE _ -> []
    InputE i -> [i]
    ConstE _ -> []
    IfThenE e te -> getInputs e <> getInputs te
    IfThenElseE e te fe -> getInputs e <> getInputs te <> getInputs fe
    EdgeSelectorE mf es -> maybe [] getInputs mf <> concatMap getInputs es
    SelectE _ selectors _ from mWhere mSplit mGroup mOrder mLimit mStart mFetch _ _ _
      -> getInputs selectors
      <> getInputs from
      <> maybe [] getInputs mWhere
      <> maybe [] getInputs mSplit
      <> maybe [] getInputs mGroup
      <> maybe [] getInputs mOrder
      <> maybe [] getInputs mLimit
      <> maybe [] getInputs mStart
      <> maybe [] getInputs mFetch
    InsertE _ tableName insertVal
      -> getInputs tableName <> getInputs insertVal
    CreateE _ target v _ _ _
      -> getInputs target <> getInputs v
    UpdateE _ target v mWhere _ _ _
      -> getInputs target <> maybe [] getInputs mWhere <> getInputs v
    DeleteE _ target mWhere _ _ _
      -> getInputs target <> maybe [] getInputs mWhere

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

instance ToQL Statement where
  toQL = \case
    UseS u -> case u of
      USE ns db -> "USE " <> ns <> " " <> db
      USE_NS ns -> "USE " <> ns
      USE_DB db -> "USE " <> db
    LetS p e -> "LET " <> toQL p <> " = " <> toQL e
    BeginS -> "BEGIN"
    CancelS -> "CANCEL"
    CommitS -> "COMMIT"
    BreakS -> "BREAK"
    ContinueS -> "CONTINUE"
    ForS p e b -> "FOR " <> toQL p <> " IN " <> toQL e <> " {" <> toQL b <> "}"

instance HasInput Statement where
  getInputs = \case
    LetS _ e -> getInputs e
    ForS _ e b -> getInputs e <> getInputs b
    _ -> []

data SurQLLine
  = ExpLine Exp
  | StatementLine Statement
  deriving (Eq, Generic, Read, Show)

instance ToQL SurQLLine where
  toQL (ExpLine e)       = toQL e
  toQL (StatementLine s) = toQL s

instance HasInput SurQLLine where
  getInputs = \case
    ExpLine e -> getInputs e
    StatementLine s -> getInputs s

newtype Block
  = Block [SurQLLine]
  deriving (Eq, Generic, Read, Show)

instance ToQL Block where
  toQL (Block ls) = prepText (intersperse ";\n" $ map toQL ls) <> ";"

instance HasInput Block where
  getInputs (Block ls) = concatMap getInputs ls

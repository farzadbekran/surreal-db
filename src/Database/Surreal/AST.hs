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

class ToQL a where
  toQL :: a -> Text

instance ToQL a => ToQL (Maybe a) where
  toQL (Just a) = toQL a
  toQL _ = ""

instance (ToQL a, ToQL b) => ToQL (Either a b) where
  toQL (Left a) = toQL a
  toQL (Right b) = toQL b

class HasInput a where
  getInputs :: a -> [Param]

instance HasInput a => HasInput (Maybe a) where
  getInputs (Just a) = getInputs a
  getInputs _ = []

instance (HasInput a, HasInput b) => HasInput (Either a b) where
  getInputs (Left a) = getInputs a
  getInputs (Right b) = getInputs b

newtype FNName
  = FNName Text
  deriving (Eq, Generic, Read, Show)

data Operator = (:&&) | (:||) | (:??) | (:?:) | (:=) | (:!=) | (:==) | (:?=) | (:*=) | (:~) | (:!~) | (:?~) | (:*~) | (:<) | (:<=) | (:>) | (:>=) | (:+) | (:-) | (:*) | (:/) | (:**) | (:+=) | (:-=) | IN | NOTIN | CONTAINS | CONTAINSNOT | CONTAINSALL | CONTAINSANY | CONTAINSNONE | INSIDE | NOTINSIDE | ALLINSIDE | ANYINSIDE | NONEINSIDE | OUTSIDE | INTERSECTS | (:@@)
  deriving (Eq, Generic, Read, Show)

instance ToQL Operator where
  toQL = \case
    (:&&) -> "AND"
    (:||) -> "OR"
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
type UserName = Text
type ScopeName = Text
type TokenName = Text
type TokenValue = Text
type EventName = Text
type FieldName = Text
type TypeName = Text
type AnalyzerName = Text
type LanguageName = Text
type IndexName = Text

type Min = Int64
type Max = Int64

type K1 = Float
type B = Float

type DefaultExp = Exp
type ValueExp = Exp
type AssertExp = Exp
type SignUpExp = Exp
type SignInExp = Exp
type AsTableViewExp = Exp
type WhenExp = Exp
type ThenExp = Exp

data TypeDef
  = T String [TypeDef]
  deriving (Eq, Generic, Read, Show)

data USE
  = USE Namespace Database
  | USE_NS Namespace
  | USE_DB Database
  deriving (Eq, Generic, Read, Show)

data Param
  = SQLParam Text
  | InputParam Text TypeDef
  deriving (Eq, Generic, Read, Show)

instance ToQL Param where
  toQL (SQLParam t) = "$" <> t
  toQL (InputParam t _) = "$" <> t

instance HasInput Param where
  getInputs (SQLParam _) = []
  getInputs i = [i]

data Field
  = WildCardField
  | SimpleField FieldName -- ^ name
  | IndexedField Field [Literal] -- ^ address[0]
  | FilteredField Field WHERE -- ^ (address WHERE city = "New York")
  | CompositeField Field Field -- ^ address.city
  | FieldParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL Field where
  toQL WildCardField = "*"
  toQL (SimpleField t) = t
  toQL (IndexedField t is) = prepText $ [toQL t] <> concatMap (\i -> ["[", toQL i, "]"]) is
  toQL (FilteredField f w) = prepText ["(", toQL f, toQL w, ")"]
  toQL (CompositeField f1 f2) = foldl1 (<>) [toQL f1, ".", toQL f2]
  toQL (FieldParam p) = toQL p

instance HasInput Field where
  getInputs = \case
    FilteredField _ w -> getInputs w
    FieldParam p -> getInputs p
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
  = FieldSelector Field
  | ExpSelector Exp Field
  | SelectorAs Selector Field
  | EdgeSelector (Maybe Field) [Edge] Field
  -- ^ f1->f2<-f3 or ->f2<-f3, last field is the alias for the results,
  -- we force it since the results are unreliable if the alias is not given!!
  | TypedSelector Selector TypeDef
  deriving (Eq, Generic, Read, Show)

instance ToQL Selector where
  toQL = \case
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
             , toQL mWith
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
  | OrderTypeParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderType where
  toQL = \case
    RAND -> "RAND"
    COLLATE -> "COLLATE"
    NUMERIC -> "NUMERIC"
    OrderTypeParam p -> toQL p

instance HasInput OrderType where
  getInputs (OrderTypeParam p) = getInputs p
  getInputs _                  = []

data OrderDirection
  = ASC
  | DESC
  | OrderDirectionParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL OrderDirection where
  toQL = \case
    ASC -> "ASC"
    DESC -> "DESC"
    OrderDirectionParam p -> toQL p

instance HasInput OrderDirection where
  getInputs (OrderDirectionParam p) = getInputs p
  getInputs _                       = []

newtype ORDER
  = ORDER [(Field, Maybe OrderType, Maybe OrderDirection)]
  deriving (Eq, Generic, Read, Show)

instance ToQL ORDER where
  toQL (ORDER []) = ""
  toQL (ORDER fs) = prepText $ ["ORDER BY"] <> intersperse "," (map renderOrder fs)
    where
      renderOrder (f, mOrderType, mOrderDirection) =
        prepText [ toQL f, toQL mOrderType, toQL mOrderDirection ]

instance HasInput ORDER where
  getInputs (ORDER os) =
    concatMap (\(f, mOtype, mOdirection)
               -> getInputs f
                <> maybe [] getInputs mOtype
                <> maybe [] getInputs mOdirection)
    os

data LIMIT
  = LIMIT Int64
  | LIMITParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL LIMIT where
  toQL (LIMIT i)      = "LIMIT " <> tshow i
  toQL (LIMITParam p) = "LIMIT " <> toQL p

instance HasInput LIMIT where
  getInputs (LIMITParam p) = getInputs p
  getInputs _              = []

data START
  = START Int64
  | STARTParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL START where
  toQL (START i)      = "START " <> tshow i
  toQL (STARTParam p) = "START " <> toQL p

instance HasInput START where
  getInputs (STARTParam p) = getInputs p
  getInputs _              = []

newtype FETCH
  = FETCH [Field]
  deriving (Eq, Generic, Read, Show)

instance ToQL FETCH where
  toQL (FETCH []) = ""
  toQL (FETCH fs) = prepText $ ["FETCH"] <> intersperse "," (map toQL fs)

instance HasInput FETCH where
  getInputs (FETCH fs) = concatMap getInputs fs

newtype TIMEOUT
  = TIMEOUT Duration
  deriving (Eq, Generic, Read, Show)

instance ToQL TIMEOUT where
  toQL (TIMEOUT d)      = "TIMEOUT " <> toQL d

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
  deriving (Eq, Generic, Read, Show)

instance ToQL TableName where
  toQL (TableName t)      = t

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
  | RecordIDParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL RecordID where
  toQL (RecordID t i)    = toQL t <> ":" <> toQL i
  toQL (RecordIDParam p) = toQL p

instance HasInput RecordID where
  getInputs (RecordIDParam p) = getInputs p
  getInputs (RecordID _ i)    = getInputs i

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
  | IDParam Param
  deriving (Eq, Generic, Read, Show)

instance ToQL ID where
  toQL = \case
    TextID t -> "`" <> t <> "`"
    NumID i -> tshow i
    ObjID o -> toQL o
    TupID es -> prepText $ ["["] <> intersperse "," (map toQL es) <> ["]"]
    RandomID fn -> toQL fn
    IDParam p -> toQL p

instance HasInput ID where
  getInputs (ObjID o)   = getInputs o
  getInputs (TupID es)  = concatMap getInputs es
  getInputs (IDParam p) = getInputs p
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
  | FieldL Field
  | ParamL Param
  | RegexL Text
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
    ParamL p -> toQL p
    FieldL f -> toQL f
    ArrayL es -> "[" <> foldl1 (<>) (intersperse "," (map toQL es)) <> "]"
    ObjectL o -> toQL o
    RecordIDL i -> toQL i
    FutureL e -> "<future> {" <> toQL e <> "}"
    RegexL r -> "/" <> r <> "/"

instance HasInput Literal where
  getInputs = \case
    ObjectL o -> getInputs o
    ArrayL es -> concatMap getInputs es
    RecordIDL i -> getInputs i
    FutureL e -> getInputs e
    RecordIDRangeL _ range -> getInputs range
    ParamL p -> getInputs p
    FieldL f -> getInputs f
    _ -> []

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
    TargetTable _ -> []
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

data RelateTarget
  = RelateTarget RecordID TableName RecordID
  deriving (Eq, Generic, Read, Show)

instance ToQL RelateTarget where
  toQL (RelateTarget rid1 tn rid2)
    = toQL rid1 <> "->" <> toQL tn <> "->" <> toQL rid2

instance HasInput RelateTarget where
  getInputs (RelateTarget rid1 _ rid2) = getInputs rid1 <> getInputs rid2

data DIFF = DIFF
  deriving (Eq, Generic, Read, Show)

instance ToQL DIFF where
  toQL _ = "DIFF"

instance HasInput DIFF where
  getInputs _ = []

data InfoParam = IPRoot | IPNS | IPDB | IPScope ScopeName | IPTable TableName
  deriving (Eq, Generic, Read, Show)

instance ToQL InfoParam where
  toQL = \case
    IPRoot -> "ROOT"
    IPNS -> "NS"
    IPDB -> "DB"
    IPScope sn -> "SCOPE " <> toQL sn
    IPTable tn -> "TABLE " <> toQL tn

data Exp
  = TypedE Exp TypeDef
  | OPE Operator Exp Exp
  | AppE FNName [Exp]
  | LitE Literal
  | ConstE Text
  | IfThenE Exp Exp
  | IfThenElseE Exp Exp Exp
  | EdgeSelectorE (Maybe Field) [Edge]
  | SelectE (Maybe VALUE) Selectors (Maybe OMIT) FROM (Maybe WHERE) (Maybe SPLIT) (Maybe GROUP) (Maybe ORDER) (Maybe LIMIT) (Maybe START) (Maybe FETCH) (Maybe TIMEOUT) (Maybe PARALLEL) (Maybe EXPLAIN)
  | LiveSelectE (Maybe VALUE) (Either DIFF Selectors) FROM (Maybe WHERE) (Maybe FETCH)
  | InsertE (Maybe IGNORE) TableName InsertVal
  | CreateE (Maybe ONLY) Target CreateVal (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | DeleteE (Maybe ONLY) Target (Maybe WHERE) (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | UpdateE (Maybe ONLY) Target UpdateVal (Maybe WHERE) (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | RelateE (Maybe ONLY) RelateTarget (Maybe UpdateVal) (Maybe ReturnType) (Maybe TIMEOUT) (Maybe PARALLEL)
  | WhereE WHERE -- ^ needed this to support table permissions
  | ReturnE Exp
  | InParenE Exp
  | InfoE InfoParam
  deriving (Eq, Generic, Read, Show)

instance ToQL Exp where
  toQL = \case
    TypedE e _ -> toQL e
    OPE op e1 e2 -> prepText [toQL e1, toQL op, toQL e2]
    AppE (FNName fnName) ps -> prepText $ [fnName <> "("] <> intersperse ", " (map toQL ps) <> [")"]
    LitE le -> toQL le
    ConstE t -> t
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
               , toQL mValue
               , toQL selectors
               , toQL mOmit
               , toQL from
               , toQL mWhere
               , toQL mSplit
               , toQL mGroup
               , toQL mOrder
               , toQL mLimit
               , toQL mStart
               , toQL mFetch
               , toQL mTimeout
               , toQL mParallel
               , toQL mExplain
               ]
    LiveSelectE mValue selectors from mWhere mFetch ->
      prepText [ "LIVE SELECT"
               , toQL mValue
               , toQL selectors
               , toQL from
               , toQL mWhere
               , toQL mFetch
               ]
    InsertE mIgnore tableName insertVal ->
      prepText [ "INSERT"
               , toQL mIgnore
               , "INTO"
               , toQL tableName
               , toQL insertVal
               ]
    CreateE mOnly target v mReturn mTimeout mParallel ->
      prepText [ "CREATE"
               , toQL mOnly
               , toQL target
               , toQL v
               , toQL mReturn
               , toQL mTimeout
               , toQL mParallel
               ]
    UpdateE mOnly target v mWhere mReturn mTimeout mParallel ->
      prepText [ "UPDATE"
               , toQL mOnly
               , toQL target
               , toQL v
               , toQL mWhere
               , toQL mReturn
               , toQL mTimeout
               , toQL mParallel
               ]
    RelateE mOnly target v mReturn mTimeout mParallel ->
      prepText [ "RELATE"
               , toQL mOnly
               , toQL target
               , toQL v
               , toQL mReturn
               , toQL mTimeout
               , toQL mParallel
               ]
    DeleteE mOnly target mWhere mReturn mTimeout mParallel ->
      prepText [ "DELETE"
               , toQL mOnly
               , toQL target
               , toQL mWhere
               , toQL mReturn
               , toQL mTimeout
               , toQL mParallel
               ]
    WhereE w -> toQL w
    ReturnE e -> "RETURN " <> toQL e
    InParenE e -> "(" <> toQL e <> ")"
    InfoE ip -> "INFO FOR " <> toQL ip

instance HasInput Exp where
  getInputs = \case
    TypedE e _ -> getInputs e
    OPE _ e1 e2 -> getInputs e1 <> getInputs e2
    AppE _ ps -> concatMap getInputs ps
    LitE le -> getInputs le
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
    LiveSelectE _ selectors from mWhere mFetch
      -> getInputs selectors
      <> getInputs from
      <> maybe [] getInputs mWhere
      <> maybe [] getInputs mFetch
    InsertE _ _ insertVal
      -> getInputs insertVal
    CreateE _ target v _ _ _
      -> getInputs target <> getInputs v
    UpdateE _ target v mWhere _ _ _
      -> getInputs target <> maybe [] getInputs mWhere <> getInputs v
    RelateE _ target v _ _ _
      -> getInputs target <> maybe [] getInputs v
    DeleteE _ target mWhere _ _ _
      -> getInputs target <> maybe [] getInputs mWhere
    WhereE w -> getInputs w
    ReturnE e -> getInputs e
    InParenE e -> getInputs e
    InfoE _ -> []

data UserScope = USROOT | USNS | USDB
  deriving (Eq, Generic, Read, Show)

instance ToQL UserScope where
  toQL = \case
    USROOT -> "ON ROOT"
    USNS -> "ON NAMESPACE"
    USDB -> "ON DATABASE"

data UserPassword
  = PASSWORD Text
  | PASSHASH Text
  deriving (Eq, Generic, Read, Show)

instance ToQL UserPassword where
  toQL = \case
    PASSWORD t -> "PASSWORD '" <> t <> "'"
    PASSHASH t -> "PASSHASH '" <> t <> "'"

data UserRole = UROWNER | UREDITOR | URVIEWER
  deriving (Eq, Generic, Read, Show)

instance ToQL UserRole where
  toQL = \case
    UROWNER -> "OWNER"
    UREDITOR -> "EDITOR"
    URVIEWER -> "VIEWER"

data TokenScope
  = TSNS
  | TSDB
  | TSScope ScopeName
  deriving (Eq, Generic, Read, Show)

instance ToQL TokenScope where
  toQL = \case
    TSNS -> "ON NAMESPACE"
    TSDB -> "ON DATABASE"
    TSScope t -> "ON SCOPE " <> t

data TokenType = EDDSA | ES256 | ES384 | ES512 | PS256 | PS384 | PS512 | RS256 | RS384 | RS512
  deriving (Eq, Generic, Read, Show)

instance ToQL TokenType where
  toQL = \case
    EDDSA -> "TYPE EDDSA"
    ES256 -> "TYPE ES256"
    ES384 -> "TYPE ES384"
    ES512 -> "TYPE ES512"
    PS256 -> "TYPE PS256"
    PS384 -> "TYPE PS384"
    PS512 -> "TYPE PS512"
    RS256 -> "TYPE RS256"
    RS384 -> "TYPE RS384"
    RS512 -> "TYPE RS512"

data DROP = DROP
  deriving (Eq, Generic, Read, Show)

instance ToQL DROP where
  toQL _ = "DROP"

data OperationType = OTSelect | OTCreate | OTUpdate | OTDelete
  deriving (Eq, Generic, Read, Show)

instance ToQL OperationType where
  toQL = \case
    OTSelect -> "select"
    OTCreate -> "create"
    OTUpdate -> "update"
    OTDelete -> "delete"

data SchemaType = SCHEMAFULL | SCHEMALESS
  deriving (Eq, Generic, Read, Show)

instance ToQL SchemaType where
  toQL = \case
    SCHEMAFULL -> "SCHEMAFULL"
    SCHEMALESS -> "SCHEMALESS"

data TablePermissions
  = TPNONE
  | TPFULL
  | TablePermissions [([OperationType], Exp)]
  deriving (Eq, Generic, Read, Show)

instance ToQL TablePermissions where
  toQL = \case
    TPNONE -> "PERMISSIONS NONE"
    TPFULL -> "PERMISSIONS FULL"
    TablePermissions perms -> prepText $
      "PERMISSIONS" : map renderPermission perms
      where
        renderPermission (ots, e) = "FOR " <> intercalate "," (map toQL ots) <> " " <> toQL e

data Flexible = Flexible
  deriving (Eq, Generic, Read, Show)

data Optional = Optional
  deriving (Eq, Generic, Read, Show)

data FieldType
  = FieldType (Maybe Flexible) (Maybe Optional) TypeName
  deriving (Eq, Generic, Read, Show)

instance ToQL FieldType where
  toQL (FieldType flex optn tn)
    = maybe "" (const "FLEXIBLE ") flex
    <> "TYPE "
    <> if isJust optn then "option<" <> toQL tn <> ">" else toQL tn

data Tokenizer
  = BLANK
  | CAMEL
  | CLASS
  | PUNCT
  deriving (Eq, Generic, Read, Show)

instance ToQL Tokenizer where
  toQL = \case
    BLANK -> "blank"
    CAMEL -> "camel"
    CLASS -> "class"
    PUNCT -> "punct"

data Filter
  = ASCII
  | Lowercase
  | Uppercase
  | Edgengram Min Max
  | Snowball LanguageName
  deriving (Eq, Generic, Read, Show)

instance ToQL Filter where
  toQL = \case
    ASCII -> "ascii"
    Lowercase -> "lowercase"
    Uppercase -> "uppercase"
    Edgengram mn mx -> "edgengram(" <> tshow mn <> ", " <> tshow mx <> ")"
    Snowball ln -> "snowball(" <> toQL ln <> ")"

data UNIQUE = UNIQUE
  deriving (Eq, Generic, Read, Show)

instance ToQL UNIQUE where
  toQL _ = "UNIQUE"

data HIGHLIGHTS = HIGHLIGHTS
  deriving (Eq, Generic, Read, Show)

instance ToQL HIGHLIGHTS where
  toQL _ = "HIGHLIGHTS"

newtype BM25 = BM25 (Maybe (K1, B))
  deriving (Eq, Generic, Read, Show)

instance ToQL BM25 where
  toQL (BM25 mParams) = "BM25" <> case mParams of
    Just (k1, b) -> "(" <> tshow k1 <> ", " <> tshow b <> ")"
    Nothing -> ""

data SearchAnalyzer = SearchAnalyzer AnalyzerName (Maybe BM25) (Maybe HIGHLIGHTS)
  deriving (Eq, Generic, Read, Show)

instance ToQL SearchAnalyzer where
  toQL (SearchAnalyzer an mBM25 mHighlights)
    = prepText
      [ "SEARCH ANALYZER"
      , toQL an
      , toQL mBM25
      , toQL mHighlights
      ]

data Define
  = DefNamespace Namespace
  | DefDatabase Database
  | DefUser UserName UserScope (Maybe UserPassword) (Maybe [UserRole])
  | DefToken TokenName TokenScope TokenType TokenValue
  | DefScope ScopeName (Maybe Duration) (Maybe SignUpExp) (Maybe SignInExp)
  | DefTable TableName (Maybe DROP) (Maybe SchemaType) (Maybe AsTableViewExp) (Maybe Duration) (Maybe TablePermissions)
  | DefEvent EventName TableName (Maybe WhenExp) ThenExp
  | DefField Field TableName (Maybe FieldType) (Maybe DefaultExp) (Maybe ValueExp) (Maybe AssertExp) (Maybe TablePermissions)
  | DefAnalyzer AnalyzerName (Maybe [Tokenizer]) (Maybe [Filter])
  | DefIndex IndexName TableName [Field] (Maybe (Either UNIQUE SearchAnalyzer))
  deriving (Eq, Generic, Read, Show)

instance ToQL Define where
  toQL = \case
    DefNamespace ns -> "DEFINE NAMESPACE " <> toQL ns
    DefDatabase db -> "DEFINE DATABASE " <> toQL db
    DefUser un level mPass mRoles
      -> prepText
         [ "DEFINE USER"
         , toQL un
         , toQL level
         , toQL mPass
         , case mRoles of
             Just rs -> prepText $ "ROLES" : intersperse "," (map toQL rs)
             Nothing -> ""
         ]
    DefToken tn ts tt tv
      -> prepText
         [ "DEFINE TOKEN"
         , toQL tn
         , toQL ts
         , toQL tt
         , "VALUE '" <> toQL tv <> "'"
         ]
    DefScope sn mDur mSignUp mSignIn
      -> prepText
         [ "DEFINE SCOPE"
         , toQL sn
         , case mDur of
             Just dur -> "SESSION " <> toQL dur
             Nothing -> ""
         , case mSignUp of
             Just signUp -> "SIGNUP " <> toQL signUp
             Nothing -> ""
         , case mSignIn of
             Just signIn -> "SIGNIN " <> toQL signIn
             Nothing -> ""
         ]
    DefTable tn mDrop mSchema mExp mDur mPerms
      -> prepText
         [ "DEFINE TABLE"
         , toQL tn
         , toQL mDrop
         , toQL mSchema
         , case mExp of
             Just e  -> "AS (" <> toQL e <> ")"
             Nothing -> ""
         , case mDur of
             Just dur -> "CHANGEFEED " <> toQL dur
             Nothing  -> ""
         , toQL mPerms
         ]
    DefEvent en tn mWhen thenE
      -> prepText
         [ "DEFINE EVENT"
         , toQL en
         , "ON TABLE"
         , toQL tn
         , case mWhen of
             Just e  -> "WHEN " <> toQL e
             Nothing -> ""
         , "THEN"
         , toQL thenE
         ]
    DefField f tn mFieldType mDefExp mValExp mAssertExp mPermissions
      -> prepText
         [ "DEFINE FIELD"
         , toQL f
         , "ON TABLE"
         , toQL tn
         , toQL mFieldType
         , case mDefExp of
             Just e  -> "DEFAULT " <> toQL e
             Nothing -> ""
         , case mValExp of
             Just e  -> "VALUE " <> toQL e
             Nothing -> ""
         , case mAssertExp of
             Just e  -> "ASSERT " <> toQL e
             Nothing -> ""
         , toQL mPermissions
         ]
    DefAnalyzer an mTokenizers mFilters
      -> prepText
         [ "DEFINE ANALYZER"
         , toQL an
         , case mTokenizers of
             Just ts -> "TOKENIZERS " <> intercalate "," (map toQL ts)
             Nothing -> ""
         , case mFilters of
             Just fs -> "FILTERS " <> intercalate "," (map toQL fs)
             Nothing -> ""
         ]
    DefIndex indxN tn fields meUSA
      -> prepText
         [ "DEFINE INDEX"
         , toQL indxN
         , "ON TABLE"
         , toQL tn
         , "FIELDS"
         , intercalate "," $ map toQL fields
         , case meUSA of
             Just (Right sa) -> toQL sa
             Just (Left u) -> toQL u
             Nothing -> ""
         ]

newtype Throw
  = Throw Exp
  deriving (Eq, Generic, Read, Show)

instance ToQL Throw where
  toQL (Throw e) = "THROW " <> toQL e

instance HasInput Throw where
  getInputs (Throw e) = getInputs e

data Statement
  = UseS USE
  | LetS Param Exp
  | BeginS
  | CancelS
  | CommitS
  | BreakS
  | ContinueS
  | ForS Param Exp Block
  | DefineS Define
  | ThrowS Throw
  deriving (Eq, Generic, Read, Show)

instance ToQL Statement where
  toQL = \case
    UseS u -> case u of
      USE ns db -> "USE NS " <> ns <> " DB " <> db
      USE_NS ns -> "USE NS " <> ns
      USE_DB db -> "USE DB " <> db
    LetS p e -> "LET " <> toQL p <> " = " <> toQL e
    BeginS -> "BEGIN"
    CancelS -> "CANCEL"
    CommitS -> "COMMIT"
    BreakS -> "BREAK"
    ContinueS -> "CONTINUE"
    ForS p e b -> "FOR " <> toQL p <> " IN " <> toQL e <> " {" <> toQL b <> "}"
    DefineS d -> toQL d
    ThrowS t -> toQL t

instance HasInput Statement where
  getInputs = \case
    LetS _ e -> getInputs e
    ForS _ e b -> getInputs e <> getInputs b
    ThrowS t -> getInputs t
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
  toQL (Block ls) = foldl1 (<>) (intersperse ";\n" $ map toQL ls) <> ";\n"

instance HasInput Block where
  getInputs (Block ls) = concatMap getInputs ls

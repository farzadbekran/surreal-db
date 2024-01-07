{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Database.Surreal.Parser where

import           ClassyPrelude                  hiding ( bool, exp, group,
                                                  index, many, some, timeout,
                                                  try )
import qualified Control.Monad.Combinators.Expr as E
import           Control.Monad.Fail             ( MonadFail (..) )
import           Data.Char                      ( isAlpha, isLower, isAlphaNum )
import           Data.Foldable                  ( foldl )
import           Data.Time.ISO8601              ( parseISO8601 )
import           Data.Void
import           Database.Surreal.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void String

-- | Consumes whitespace and newlines
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Eats whitespace after the parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse the exact String given
symbol :: String -> Parser Text
symbol s = pack <$> L.symbol sc s

-- | Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveSymbol :: String -> Parser String
caseInsensitiveSymbol s = label s $ lexeme $ L.symbol' sc s

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme $ char '(') (lexeme $ char ')')

maybeBetweenParens :: Parser a -> Parser a
maybeBetweenParens p = p <|> betweenParens p

identifier :: Parser Identifier
identifier = lexeme $ do
  s <- pack <$> takeWhile1P Nothing isValidIdentifierChar
  case mkIdentifier s of
    Just i -> return i
    _      -> fail $ "invalid identifier: " <> unpack s

-- | Type Parsers

typeVar :: Parser Text
typeVar = lexeme $ do
  c <- takeWhile1P Nothing isAlpha
  case c of
    [c'] -> if isLower c'
      then return $ pack c
      else fail "Invalid type varible!"
    _otherwise -> fail "Invalid type varible!"

nestedType :: Parser TypeDef
nestedType = lexeme $ do
  prefix <- lexeme $ takeWhile1P Nothing isAlphaNum
  postfix <- many parseType
  return $ T prefix postfix

simpleType :: Parser TypeDef
simpleType = lexeme $ choice
  [ do
      t <- lexeme $ takeWhile1P Nothing isAlphaNum
      return $ T t []
  , do
      _ <- symbol "()"
      return $ T "()" []
  ]

parseType :: Parser TypeDef
parseType = lexeme $ try (betweenParens nestedType) <|> simpleType

-- | Literals

noneL :: Parser Literal
noneL = label "NONE" $ lexeme $ caseInsensitiveSymbol "NONE" $> NoneL

nullL :: Parser Literal
nullL = label "NULL" $ lexeme $ caseInsensitiveSymbol "NULL" $> NoneL

boolL :: Parser Literal
boolL = label "Bool" $ lexeme $
  BoolL False <$ caseInsensitiveSymbol "False" <|>
  BoolL False <$ caseInsensitiveSymbol "True"

quotedText :: Parser Text
quotedText = label "quotedText" $ lexeme $ pack <$>
  (between (char '"') (char '"') (takeWhileP Nothing (/= '"'))
    <|> between (char '\'') (char '\'') (takeWhileP Nothing (/= '\'')))

textL :: Parser Literal
textL = label "TextL" $ lexeme $ TextL <$> quotedText

intParser :: Parser Int64
intParser = label "Int64" $ do
  s <- some numberChar
  let mr = readMay s :: Maybe Int64
    in
    case mr of
      Just r  -> return r
      Nothing -> fail "Invalid Int64"

floatParser :: Parser Float
floatParser = label "Float" $ do
  s <- some numberChar <> (unpack <$> symbol ".") <> some numberChar
  let mr = readMay s :: Maybe Float
    in
    case mr of
      Just r  -> return r
      Nothing -> fail "Invalid Float"

int64L :: Parser Literal
int64L = label "Int64L" $ lexeme $ Int64L <$> L.signed sc intParser

floatL :: Parser Literal
floatL = label "FloatL" $ lexeme $ FloatL <$> L.signed sc floatParser

utcTimeParser :: Parser UTCTime
utcTimeParser = label "utcTimeParser" $ lexeme $ do
  s <- unpack <$> quotedText
  let mUTC = parseISO8601 s
  case mUTC of
    Just utc -> return utc
    _        -> fail "Invalid ISO8601 DateTime"

dateTimeL :: Parser Literal
dateTimeL = label "dateTimeL" $ lexeme $ DateTimeL <$> utcTimeParser

durationParser :: Parser Duration
durationParser = label "Duration" $ lexeme $ do
  let
    tags = ["y", "w", "d", "h", "m", "s", "ms", "us", "ns"]
    partParser = do
      intPart <- intParser
      tagPart <- choice $ map string tags
      return (intPart, tagPart)
  parts <- some partParser
  let duration = foldl (\dur (i, t) -> case t of
                           "y"  -> dur { _y = _y dur + i}
                           "w"  -> dur { _w = _w dur + i}
                           "d"  -> dur { _d = _d dur + i}
                           "h"  -> dur { _h = _h dur + i}
                           "m"  -> dur { _m = _m dur + i}
                           "s"  -> dur { _s = _s dur + i}
                           "ms" -> dur { _ms = _ms dur + i}
                           "us" -> dur { _us = _us dur + i}
                           "ns" -> dur { _ns = _ns dur + i}
                           _    -> dur
                       )
                 defaultDuration parts
  return duration

durationL :: Parser Literal
durationL = label "DurationL" $ lexeme $ DurationL <$> durationParser

paramL :: Parser Literal
paramL = label "literalParam" $ lexeme $ ParamL <$> param

textID :: Parser ID
textID = label "TextID" $ lexeme $ do
  t <- pack <$> between (char '`') (char '`') (takeWhileP Nothing (/= '`'))
       <|> pack <$> between (char '⟨') (char '⟩') (takeWhileP Nothing (/= '⟩'))
       <|> pack <$> some alphaNumChar
  return $ TextID t

numID :: Parser ID
numID = label "numID" $ lexeme $ do
  i <- intParser
  -- just making sure the next char is not a alphabetic Char
  -- oterwise it messes with the textID parser
  lookAhead $ space1 <|> (symbol "->" $> ()) <|> (symbol "<-" $> ()) <|> eof
  return $ NumID i

objID :: Parser ID
objID = label "objID" $ lexeme $ ObjID <$> object_

objectField :: Parser (Field, Exp)
objectField = label "objectField" $ lexeme $ do
  f <- lexeme $ field <|> between (char '"') (char '"') field
  _ <- lexeme $ symbol ":"
  e <- exp
  return (f,e)

object_ :: Parser Object
object_ = label "Object" $ lexeme $ do
  fields <- between (lexeme $ char '{') (lexeme $ char '}') $ sepBy objectField (lexeme $ char ',')
  return $ Object fields

tupID :: Parser ID
tupID = label "tupID" $ lexeme
  $ TupID <$> between (char '[') (char ']')
  (sepBy exp (lexeme $ char ','))

randomID :: Parser ID
randomID = label "randomID" $ lexeme $ choice
  [ caseInsensitiveSymbol "rand()" $> RandomID RNRAND
  , caseInsensitiveSymbol "ulid()" $> RandomID RNULID
  , caseInsensitiveSymbol "uuid()" $> RandomID RNUUID
  ]

idParam :: Parser ID
idParam = label "idParam" $ lexeme $ IDParam <$> param

id_ :: Parser ID
id_ = label "ID" $ choice $ map try
  [ randomID
  , numID
  , textID
  , objID
  , tupID
  , idParam
  ]

normalRecordID :: Parser RecordID
normalRecordID = label "normalRecordID" $ lexeme $ do
  tn <- tableName
  _ <- symbol ":"
  RecordID tn <$> id_

recordIDParam :: Parser RecordID
recordIDParam = label "recordIDParam" $ lexeme $ do
  p <- param
  lookAhead space1 -- need to make sure we are not parsing a $p.somefield by mistake
  return $ RecordIDParam p

recordID :: Parser RecordID
recordID = label "RecordID" $ lexeme $ choice
  [ normalRecordID
  , recordIDParam
  ]

recordIDL :: Parser Literal
recordIDL = label "RecordIDL" $ lexeme $ RecordIDL <$> recordID

idRangeLT :: Parser IDRange
idRangeLT = label "idRangeLT" $ do
  _ <- string ".."
  IDRangeLT <$> id_

idRangeGT :: Parser IDRange
idRangeGT = label "idRangeGT" $ do
  i <- id_
  _ <- string ".."
  return $ IDRangeGT i

idRangeBetween :: Parser IDRange
idRangeBetween = label "idRangeBetween" $ do
  i1 <- id_
  _ <- string ".."
  IDRangeBetween i1 <$> id_

idRange :: Parser IDRange
idRange = label "IDRange" $ lexeme $ choice $ map try
  [ idRangeBetween
  , idRangeGT
  , idRangeLT
  ]

recordIDRangeL :: Parser Literal
recordIDRangeL = label "recordIDRageL" $ lexeme $ do
  tn <- tableName
  _ <- string ":"
  RecordIDRangeL tn <$> idRange

objectL :: Parser Literal
objectL = label "objectL" $ lexeme $ ObjectL <$> object_

arrayL :: Parser Literal
arrayL = label "arrayL" $ lexeme $ do
  es <- between (char '[') (char ']') $ sepBy exp (char ',')
  return $ ArrayL es

futureL :: Parser Literal
futureL = label "futureL" $ lexeme $ do
  _ <- caseInsensitiveSymbol "<future>"
  e <- between (char '{') (char '}') exp
  return $ FutureL e

regexL :: Parser Literal
regexL = label "regexL" $ lexeme $ RegexL . pack <$>
  (char '/' *> manyTill L.charLiteral (char '/'))

fieldL :: Parser Literal
fieldL = label "fieldL" $ lexeme $ FieldL <$> field

-- | TODO: add missing types like location/coordinates
literal :: Parser Literal
literal = lexeme $ maybeBetweenParens $ choice $ map try
  [ noneL
  , nullL
  , boolL
  , dateTimeL
  , durationL
  , floatL
  , textL
  , int64L
  , regexL
  , recordIDRangeL
  , recordIDL
  , fieldL
  , paramL
  , objectL
  , arrayL
  , futureL
  ]

wildCardField :: Parser Field
wildCardField = label "wildCardField" $ lexeme $ symbol "*" $> WildCardField

simpleField :: Parser Field
simpleField = label "SimpleField" $ lexeme $ SimpleField . FieldName <$> identifier

indexedField :: Parser (Field -> Field)
indexedField = label "IndexedField" $ lexeme $ do
  is <- many $ between (char '[') (char ']') literal
  if null is
    then
    fail "Empty index!"
    else
    return (`IndexedField` is)

filteredField :: Parser Field
filteredField = label "FilteredField" $ lexeme $ betweenParens $ do
  f <- field
  FilteredField f <$> where_

fieldOperatorTable :: [[E.Operator Parser Field]]
fieldOperatorTable = [ [ E.Postfix indexedField
                       , E.InfixL (symbol "." $> CompositeField)
                       ] ]

fieldParam :: Parser Field
fieldParam = label "fieldParam" $ FieldParam <$> param

field :: Parser Field
field = E.makeExprParser fieldTerm fieldOperatorTable

fieldTerm :: Parser Field
fieldTerm = lexeme $ choice $ map try
  [ fieldParam
  , filteredField
  , simpleField
  , wildCardField
  ]

fnName :: Parser FNName
fnName = label "FNName" $ lexeme $ do
  parts <- sepBy identifier (symbol "::")
  if null parts
    then fail "Invalid function name."
    else
      return $ FNName parts

appE :: Parser Exp
appE = label "AppE" $ lexeme $ do
  fnn <- fnName
  exps <- betweenParens $ sepBy exp (lexeme $ char ',')
  return $ AppE fnn exps

litE :: Parser Exp
litE = label "LitE" $ lexeme $ LitE <$> literal

-- | TODO: fix this
constE :: Parser Exp
constE = label "ConstE" $ lexeme $ ConstE <$> identifier

param :: Parser Param
param = label "Param" $ lexeme $ choice
  [ do _ <- char '$'
       SQLParam . ParamName <$> identifier
  , do
      _ <- char '%'
      n <- ParamName <$> identifier
      _ <- lexeme $ symbol "::"
      InputParam n <$> parseType
  ]

ifThenE :: Parser Exp
ifThenE = label "IfThenE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "IF"
  e1 <- exp
  _ <- optional $ caseInsensitiveSymbol "THEN"
  IfThenE e1 <$> exp

ifThenElseE :: Parser Exp
ifThenElseE = label "IfThenElseE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "IF"
  e1 <- exp
  _ <- optional $ caseInsensitiveSymbol "THEN"
  e2 <- exp
  _ <- caseInsensitiveSymbol "ELSE"
  IfThenElseE e1 e2 <$> exp

parseCapitalWord :: Parser Text
parseCapitalWord = lexeme $ do
  initialLetter <- upperChar
  rest <- identifier
  return $ pack $ initialLetter : unpack (toQL rest)

typeCon :: Parser TypeDef
typeCon = do
  w <- try parseCapitalWord
  return $ T (unpack w) []

outEdge :: Parser Edge
outEdge = label "OutEdge" $ lexeme $ do
  _ <- symbol "->"
  OutEdge <$> field

inEdge :: Parser Edge
inEdge = label "InEdge" $ lexeme $ do
  _ <- symbol "<-"
  InEdge <$> field

edge :: Parser Edge
edge = label "Edge" $ lexeme $ choice [outEdge, inEdge]

edgeSelector :: Parser Selector
edgeSelector = label "EdgeSelector" $ lexeme $ do
  mInitialField <- optional field
  edges <- some edge
  _ <- caseInsensitiveSymbol "AS"
  f <- field
  if null edges
    then fail "Invalid Edge!"
    else return $ EdgeSelector mInitialField edges f

fieldSelector :: Parser Selector
fieldSelector = label "FieldSelector" $ FieldSelector <$> field

expSelector :: Parser Selector
expSelector = label "ExpSelector" $ lexeme $ do
  e <- exp
  _ <- caseInsensitiveSymbol "AS"
  ExpSelector e <$> simpleField

selectorAs :: Parser (Selector -> Selector)
selectorAs = label "SelectorAs" $ lexeme $ do
  _ <- caseInsensitiveSymbol "AS"
  f <- simpleField
  return (`SelectorAs` f)

typedSelector :: Parser (Selector -> Selector)
typedSelector = do
  _ <- lexeme $ symbol "::"
  t <- parseType
  return (`TypedSelector` t)

-- order matters here, more specific first, ie ** before *
selectorOperatorTable :: [[E.Operator Parser Selector]]
selectorOperatorTable = [ [ E.Postfix selectorAs
                          ]
                        , [ E.Postfix typedSelector
                          ]
                        ]

selector :: Parser Selector
selector = E.makeExprParser selectorTerm selectorOperatorTable

selectorTerm :: Parser Selector
selectorTerm = lexeme $ choice $ map try
  [ fieldSelector
  , expSelector
  , edgeSelector
  ]

selectors :: Parser Selectors
selectors = lexeme $ Selectors <$> sepBy selector (lexeme $ char ',')

omit :: Parser OMIT
omit = label "OMIT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "OMIT"
  OMIT <$> sepBy field (lexeme $ char ',')

index :: Parser INDEX
index = label "INDEX" $ lexeme $ noindex <|> indexes
  where
    noindex = caseInsensitiveSymbol "NOINDEX" $> NOINDEX
    indexes = INDEX <$> sepBy (IndexName <$> identifier) (char ',')

with :: Parser WITH
with = label "WITH" $ lexeme $ do
  _ <- caseInsensitiveSymbol "WITH"
  WITH <$> index

from :: Parser FROM
from = label "FROM" $ lexeme $ do
  _ <- caseInsensitiveSymbol "FROM"
  mOnly <- optional $ caseInsensitiveSymbol "ONLY" $> ONLY
  e <- exp
  mWith <- optional with
  return $ FROM mOnly e mWith

where_ :: Parser WHERE
where_ = label "WHERE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "WHERE"
  WHERE <$> exp

split :: Parser SPLIT
split = label "SPLIT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SPLIT"
  _ <- optional $ caseInsensitiveSymbol "AT"
  SPLIT <$> sepBy field (lexeme $ char ',')

group :: Parser GROUP
group = label "GROUP" $ lexeme $ do
  _ <- caseInsensitiveSymbol "GROUP"
  _ <- optional $ caseInsensitiveSymbol "BY"
  GROUP <$> sepBy field (lexeme $ char ',')

orderType :: Parser OrderType
orderType = label "OrderType" $ lexeme $ do
  caseInsensitiveSymbol "RAND" $> RAND
  <|> caseInsensitiveSymbol "COLLATE" $> COLLATE
  <|> caseInsensitiveSymbol "NUMERIC" $> NUMERIC
  <|> OrderTypeParam <$> param

orderDirection :: Parser OrderDirection
orderDirection = label "OrderDirection" $ lexeme $ do
  caseInsensitiveSymbol "ASC" $> ASC
  <|> caseInsensitiveSymbol "DESC" $> DESC
  <|> OrderDirectionParam <$> param

order :: Parser ORDER
order = label "ORDER" $ lexeme $ do
  _ <- caseInsensitiveSymbol "ORDER"
  _ <- optional $ caseInsensitiveSymbol "BY"
  ORDER <$> sepBy orderPart (lexeme $ char ',')
  where
    orderPart = (,,) <$> field <*> optional orderType <*> optional orderDirection

limit :: Parser LIMIT
limit = label "LIMIT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "LIMIT"
  _ <- optional $ caseInsensitiveSymbol "BY"
  LIMIT <$> intParser <|> LIMITParam <$> param

start :: Parser START
start = label "START" $ lexeme $ do
  _ <- caseInsensitiveSymbol "START"
  _ <- optional $ caseInsensitiveSymbol "AT"
  START <$> intParser <|> STARTParam <$> param

fetch :: Parser FETCH
fetch = label "FETCH" $ lexeme $ do
  _ <- caseInsensitiveSymbol "FETCH"
  FETCH <$> sepBy field (char ',')

timeout :: Parser TIMEOUT
timeout = label "TIMEOUT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "TIMEOUT"
  TIMEOUT <$> durationParser

parallel :: Parser PARALLEL
parallel = label "PARALLEL" $ caseInsensitiveSymbol "PARALLEL" $> PARALLEL

explain :: Parser EXPLAIN
explain = label "EXPLAIN" $ lexeme $ do
  _ <- caseInsensitiveSymbol "EXPLAIN"
  mFull <- optional $ caseInsensitiveSymbol "FULL"
  return $ if isJust mFull then EXPLAINFULL else EXPLAIN

tableName :: Parser TableName
tableName = label "TableName" $ lexeme $ TableName <$> identifier

scopeName :: Parser ScopeName
scopeName = label "ScopeName" $ lexeme $ ScopeName <$> identifier

selectE :: Parser Exp
selectE = label "SelectE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SELECT"
  mValue <- optional $ caseInsensitiveSymbol "VALUE" $> VALUE
  sels <- selectors
  mOmit <- optional omit
  from_ <- from
  mWhere <- optional where_
  mSplit <- optional split
  mGroup <- optional group
  mOrder <- optional order
  mLimit <- optional limit
  mStart <- optional start
  mFetch <- optional fetch
  mTimeout <- optional timeout
  mParallel <- optional parallel
  mExplain <- optional explain
  return $ SelectE mValue sels mOmit from_ mWhere mSplit mGroup mOrder mLimit mStart mFetch mTimeout mParallel mExplain

liveSelectE :: Parser Exp
liveSelectE = label "LiveSelectE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "LIVE"
  _ <- caseInsensitiveSymbol "SELECT"
  mValue <- optional $ caseInsensitiveSymbol "VALUE" $> VALUE
  sels <- do
    ss <- optional $ try selectors
    diff <- optional $ try $ caseInsensitiveSymbol "DIFF"
    case (ss,diff) of
      (Just s, Nothing) -> return $ Right s
      (Nothing, Just _) -> return $ Left DIFF
      _                 -> fail "failed to parse selectors for live select!"
  from_ <- from
  mWhere <- optional where_
  mFetch <- optional fetch
  return $ LiveSelectE mValue sels from_ mWhere mFetch

onDuplicate :: Parser OnDuplicate
onDuplicate = label "onDuplicate" $ lexeme $ do
  _ <- mapM caseInsensitiveSymbol ["ON", "DUPLICATE", "KEY", "UPDATE"]
  es <- sepBy exp (char ',')
  return $ OnDuplicate es

insertObject :: Parser InsertVal
insertObject = label "insertObject" $ lexeme $ do
  os <- (:[]) <$> object_
        <|> between (lexeme $ char '[') (lexeme $ char ']') (sepBy object_ (lexeme $ char ','))
  return $ InsertObjects os

insertValues :: Parser InsertVal
insertValues = label "insertValues" $ lexeme $ do
  fs <- lexeme $ betweenParens $ sepBy field (lexeme $ char ',')
  _ <- caseInsensitiveSymbol "VALUES"
  vs <- sepBy tupleParser (lexeme $ char ',')
  let fieldsLength = length fs
  unless (all (\item -> length item == fieldsLength) vs)
    (fail "Length of inserted values don't match the field count!")
  mOnDuplicate <- optional onDuplicate
  return $ InsertValues fs vs mOnDuplicate
  where
    tupleParser = lexeme $ betweenParens $ sepBy exp (lexeme $ char ',')

insertVal :: Parser InsertVal
insertVal = label "insertVal" $ lexeme $ choice $ map try
  [ insertObject
  , insertValues
  , InsertParam <$> param
  ]

insertE :: Parser Exp
insertE = label "insertE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "INSERT"
  mIgnore <- optional $ caseInsensitiveSymbol "IGNORE" $> IGNORE
  _ <- caseInsensitiveSymbol "INTO"
  t <- target
  InsertE mIgnore t <$> insertVal

targetEdge :: Parser Target
targetEdge = label "targetEdge" $ lexeme $ do
  initialID <- recordID
  edges <- some edge
  if null edges
    then fail "Invalid Edge!"
    else return $ TargetEdge initialID edges

target :: Parser Target
target = label "target" $ lexeme $ choice $ map try
  [ targetEdge
  , TargetRecID <$> recordID
  , TargetTable <$> tableName
  , TargetParam <$> param
  ]

createObject :: Parser CreateVal
createObject = label "createObject" $ lexeme $ do
  _ <- caseInsensitiveSymbol "CONTENT"
  CreateObject <$> exp

createValues :: Parser CreateVal
createValues = label "createValues" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SET"
  fields <- sepBy parseField (lexeme $ char ',')
  return $ CreateValues fields
  where
    parseField = do
      f <- field
      _ <- lexeme $ char '='
      e <- exp
      return (f,e)

createVal :: Parser CreateVal
createVal = label "createVal" $ lexeme $ choice
  [ createObject
  , createValues
  ]

updateObject :: Parser UpdateVal
updateObject = label "updateObject" $ lexeme $ do
  _ <- caseInsensitiveSymbol "CONTENT"
  UpdateObject <$> exp

updateMerge :: Parser UpdateVal
updateMerge = label "updateMerge" $ lexeme $ do
  _ <- caseInsensitiveSymbol "MERGE"
  UpdateMerge <$> exp

updatePatch :: Parser UpdateVal
updatePatch = label "updatePatch" $ lexeme $ do
  _ <- caseInsensitiveSymbol "PATCH"
  UpdatePatch <$> exp

updateValues :: Parser UpdateVal
updateValues = label "updateValues" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SET"
  fields <- sepBy parseField (lexeme $ char ',')
  return $ UpdateValues fields
  where
    parseField = do
      f <- field
      _ <- lexeme $ char '='
      e <- exp
      return (f,e)

updateVal :: Parser UpdateVal
updateVal = label "updateVal" $ lexeme $ choice
  [ updateObject
  , updateValues
  , updateMerge
  , updatePatch
  ]

returnType :: Parser ReturnType
returnType = label "ReturnType" $ lexeme
  $ caseInsensitiveSymbol "RETURN"
  >> choice (map try
             [ caseInsensitiveSymbol "NONE" $> RTNone
             , caseInsensitiveSymbol "BEFORE" $> RTBefore
             , caseInsensitiveSymbol "AFTER" $> RTAfter
             , caseInsensitiveSymbol "DIFF" $> RTDiff
             , RTProjections <$> selectors
             ])

createE :: Parser Exp
createE = label "createE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "CREATE"
  mOnly <- optional $ caseInsensitiveSymbol "ONLY" $> ONLY
  tar <- target
  val <- createVal
  mReturn <- optional returnType
  mTimeout <- optional timeout
  mParallel <- optional parallel
  return $ CreateE mOnly tar val mReturn mTimeout mParallel

updateE :: Parser Exp
updateE = label "updateE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "UPDATE"
  mOnly <- optional $ caseInsensitiveSymbol "ONLY" $> ONLY
  tar <- target
  val <- updateVal
  mWhere <- optional where_
  mReturn <- optional returnType
  mTimeout <- optional timeout
  mParallel <- optional parallel
  return $ UpdateE mOnly tar val mWhere mReturn mTimeout mParallel

relateTarget :: Parser RelateTarget
relateTarget = label "RelateTarget" $ lexeme $ do
  rid1 <- recordID
  _ <- symbol "->"
  tn <- tableName
  _ <- symbol "->"
  RelateTarget rid1 tn <$> recordID

relateE :: Parser Exp
relateE = label "relateE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "RELATE"
  mOnly <- optional $ caseInsensitiveSymbol "ONLY" $> ONLY
  tar <- relateTarget
  val <- optional updateVal
  mReturn <- optional returnType
  mTimeout <- optional timeout
  mParallel <- optional parallel
  return $ RelateE mOnly tar val mReturn mTimeout mParallel

deleteE :: Parser Exp
deleteE = label "deleteE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DELETE"
  mOnly <- optional $ caseInsensitiveSymbol "ONLY" $> ONLY
  tar <- target
  mWhere <- optional where_
  mReturn <- optional returnType
  mTimeout <- optional timeout
  mParallel <- optional parallel
  return $ DeleteE mOnly tar mWhere mReturn mTimeout mParallel

typedExp :: Parser (Exp -> Exp)
typedExp = do
  _ <- lexeme $ symbol "::"
  t <- parseType
  return (`TypedE` t)

edgeSelectorE :: Parser Exp
edgeSelectorE = label "edgeSelectorE" $ lexeme $ do
  mInitialField <- optional field
  edges <- some edge
  if null edges
    then fail "Invalid Edge!"
    else return $ EdgeSelectorE mInitialField edges

returnE :: Parser Exp
returnE = label "returnE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "RETURN"
  ReturnE <$> exp

infoParam :: Parser InfoParam
infoParam = label "infoParam" $ lexeme $ do
  choice $ map try
    [ caseInsensitiveSymbol "ROOT" $> IPRoot
    , (caseInsensitiveSymbol "NS" <|> caseInsensitiveSymbol "NAMESPACE") $> IPNS
    , (caseInsensitiveSymbol "DB" <|> caseInsensitiveSymbol "DATABASE") $> IPDB
    , (caseInsensitiveSymbol "TABLE" >> tableName) <&> IPTable
    , (caseInsensitiveSymbol "SCOPE" >> scopeName) <&> IPScope
    ]

infoE :: Parser Exp
infoE = label "infoE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "INFO"
  _ <- caseInsensitiveSymbol "FOR"
  InfoE <$> infoParam

timeStamp :: Parser TimeStamp
timeStamp = label "timeStamp" $ lexeme $ do
  choice $ map try
    [ TimeStamp <$> utcTimeParser
    , TimeStampParam <$> param
    ]

showChangesE :: Parser Exp
showChangesE = label "showChangesE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SHOW"
  _ <- caseInsensitiveSymbol "CHANGES"
  _ <- caseInsensitiveSymbol "FOR"
  _ <- caseInsensitiveSymbol "TABLE"
  tn <- tableName
  mSince <- optional $ caseInsensitiveSymbol "SINCE" >> timeStamp
  mLimit <- optional limit
  return $ ShowChangesE tn mSince mLimit

-- order matters here, more specific first, ie ** before *
operatorTable :: [[E.Operator Parser Exp]]
operatorTable = [ [ E.Postfix typedExp
                  , E.InfixN (symbol "**" $> OPE (:**))
                  , E.InfixN (symbol "??" $> OPE (:??))
                  , E.InfixN (symbol "?:" $> OPE (:?:))
                  , E.InfixN (symbol "==" $> OPE (:==))
                  , E.InfixN (symbol "!=" $> OPE (:!=))
                  , E.InfixN (symbol "?=" $> OPE (:?=))
                  , E.InfixN (symbol "*=" $> OPE (:*=))
                  , E.InfixN (symbol "!~" $> OPE (:!~))
                  , E.InfixN (symbol "?~" $> OPE (:?~))
                  , E.InfixN (symbol "*~" $> OPE (:*~))
                  , E.InfixN (symbol "<=" $> OPE (:<=))
                  , E.InfixN (symbol ">=" $> OPE (:>=))
                  , E.InfixN (symbol "+=" $> OPE (:+=))
                  , E.InfixN (symbol "-=" $> OPE (:-=))
                  , E.InfixN (symbol ">" $> OPE (:>))
                  , E.InfixN (symbol "+" $> OPE (:+))
                  , E.InfixN (symbol "-" $> OPE (:-))
                  , E.InfixN (symbol "*" $> OPE (:*))
                  , E.InfixN (symbol "/" $> OPE (:/))
                  , E.InfixN (symbol "<" $> OPE (:<))
                  , E.InfixN (symbol "=" $> OPE (:=))
                  , E.InfixN (symbol "~" $> OPE (:~))
                  , E.InfixN (symbol "INSIDE" $> OPE INSIDE)
                  , E.InfixN (symbol "NOTINSIDE" $> OPE NOTINSIDE)
                  , E.InfixN (symbol "ALLINSIDE" $> OPE ALLINSIDE)
                  , E.InfixN (symbol "ANYINSIDE" $> OPE ANYINSIDE)
                  , E.InfixN (symbol "NONEINSIDE" $> OPE NONEINSIDE)
                  , E.InfixN (symbol "IN" $> OPE IN)
                  , E.InfixN (symbol "NOTIN" $> OPE NOTIN)
                  , E.InfixN (symbol "CONTAINSNOT" $> OPE CONTAINSNOT)
                  , E.InfixN (symbol "CONTAINSALL" $> OPE CONTAINSALL)
                  , E.InfixN (symbol "CONTAINSANY" $> OPE CONTAINSANY)
                  , E.InfixN (symbol "CONTAINSNONE" $> OPE CONTAINSNONE)
                  , E.InfixN (symbol "CONTAINS" $> OPE CONTAINS)
                  , E.InfixN (symbol "OUTSIDE" $> OPE OUTSIDE)
                  , E.InfixN (symbol "INTERSECTS" $> OPE INTERSECTS)
                  , E.InfixN (symbol "@@" $> OPE (:@@))
                  ]
                , [ E.InfixN (symbol "&&" $> OPE (:&&))
                  , E.InfixN (symbol "AND" $> OPE (:&&))
                  , E.InfixN (symbol "||" $> OPE (:||))
                  , E.InfixN (symbol "OR" $> OPE (:||))
                  ]
                ]

exp :: Parser Exp
exp = E.makeExprParser term operatorTable

term :: Parser Exp
term = sc
  >> lexeme (choice $ map try
              [ ifThenE
              , ifThenElseE
              , selectE
              , liveSelectE
              , insertE
              , createE
              , updateE
              , relateE
              , deleteE
              , returnE
              , infoE
              , WhereE <$> where_
              , showChangesE
              , appE
              , edgeSelectorE
              , litE
              , InParenE <$> betweenParens exp
              , BlockE <$> between (lexeme $ char '{') (lexeme $ char '}') block
              , constE
              ])

useNSDB :: Parser Statement
useNSDB = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "NS"
  ns <- Namespace <$> identifier
  _ <- caseInsensitiveSymbol "DB"
  UseS . USE ns . Database <$> identifier

useNS :: Parser Statement
useNS = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "NS"
  UseS . USE_NS . Namespace <$> identifier

useDB :: Parser Statement
useDB = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "DB"
  UseS . USE_DB . Database <$> identifier

useS :: Parser Statement
useS = label "useS" $ lexeme $ choice $ map try
  [ useNSDB
  , useNS
  , useDB
  ]

letS :: Parser Statement
letS = label "letS" $ lexeme $ do
  _ <- caseInsensitiveSymbol "LET"
  p <- param
  _ <- symbol "="
  LetS p <$> exp

forS :: Parser Statement
forS = label "forS" $ lexeme $ do
  _ <-  caseInsensitiveSymbol "FOR"
  p <- param
  _ <- caseInsensitiveSymbol "IN"
  e <- exp
  ForS p e <$> block

defNamespace :: Parser Define
defNamespace = label "defNamespace" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "NAMESPACE"
  DefNamespace . Namespace <$> identifier

defDatabase :: Parser Define
defDatabase = label "defDatabase" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "DATABASE"
  DefDatabase . Database <$> identifier

userScope :: Parser UserScope
userScope = label "userScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "ON"
  choice [ caseInsensitiveSymbol "ROOT" $> USROOT
         , caseInsensitiveSymbol "NAMESPACE" $> USNS
         , caseInsensitiveSymbol "DATABASE" $> USDB
         ]

userPass :: Parser UserPassword
userPass = label "userPass" $ lexeme $ do
  choice [ caseInsensitiveSymbol "PASSWORD" >> quotedText <&> PASSWORD
         , caseInsensitiveSymbol "PASSHASH" >> quotedText <&> PASSHASH
         ]

userRole :: Parser UserRole
userRole = label "userRole" $ lexeme $ do
  choice [ caseInsensitiveSymbol "OWNER" $> UROWNER
         , caseInsensitiveSymbol "EDITOR" $> UREDITOR
         , caseInsensitiveSymbol "VIEWER" $> URVIEWER
         ]

defUser :: Parser Define
defUser = label "defUser" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "USER"
  un <- identifier
  scope <- userScope
  pass <- optional userPass
  roles <- optional $ caseInsensitiveSymbol "ROLES" >> sepBy userRole (lexeme $ char ',')
  return $ DefUser (UserName un) scope pass roles

tokenScope :: Parser TokenScope
tokenScope = label "tokenScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "ON"
  choice [ caseInsensitiveSymbol "NAMESPACE" $> TSNS
         , caseInsensitiveSymbol "DATABASE" $> TSDB
         , caseInsensitiveSymbol "SCOPE" >> (ScopeName <$> identifier) <&> TSScope
         ]

tokenType :: Parser TokenType
tokenType = label "tokenType" $ lexeme $ choice $ map try
  [ caseInsensitiveSymbol "EDDSA" $> EDDSA
  , caseInsensitiveSymbol "ES256" $> ES256
  , caseInsensitiveSymbol "ES384" $> ES384
  , caseInsensitiveSymbol "ES512" $> ES512
  , caseInsensitiveSymbol "PS256" $> PS256
  , caseInsensitiveSymbol "PS384" $> PS384
  , caseInsensitiveSymbol "PS512" $> PS512
  , caseInsensitiveSymbol "RS256" $> RS256
  , caseInsensitiveSymbol "RS384" $> RS384
  , caseInsensitiveSymbol "RS512" $> RS512
  ]

defToken :: Parser Define
defToken = label "defToken" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "TOKEN"
  tn <- identifier
  scope <- tokenScope
  _ <- caseInsensitiveSymbol "TYPE"
  t <- tokenType
  _ <- caseInsensitiveSymbol "VALUE"
  DefToken (TokenName tn) scope t <$> quotedText

defScope :: Parser Define
defScope = label "defScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "SCOPE"
  sn <- identifier
  mDur <- optional $ caseInsensitiveSymbol "SESSION" >> durationParser
  mSignUp <- optional $ caseInsensitiveSymbol "SIGNUP" >> exp
  mSignIn <- optional $ caseInsensitiveSymbol "SIGNIN" >> exp
  return $ DefScope (ScopeName sn) mDur mSignUp mSignIn

operationType :: Parser OperationType
operationType = label "operationType" $ lexeme $ choice
  [ caseInsensitiveSymbol "SELECT" $> OTSelect
  , caseInsensitiveSymbol "CREATE" $> OTCreate
  , caseInsensitiveSymbol "UPDATE" $> OTUpdate
  , caseInsensitiveSymbol "DELETE" $> OTDelete
  ]

tablePermissionsFor :: Parser TablePermissions
tablePermissionsFor = label "tablePermissionsFor" $ lexeme $ do
  ps <- some $ do
    _ <- caseInsensitiveSymbol "FOR"
    ots <- sepBy operationType (lexeme $ char ',')
    e <- exp
    return (ots, e)
  return $ TablePermissions ps

tablePermissions :: Parser TablePermissions
tablePermissions = label "tablePermissions" $ lexeme $ do
  _ <- caseInsensitiveSymbol "PERMISSIONS"
  choice
    [ caseInsensitiveSymbol "NONE" $> TPNONE
    , caseInsensitiveSymbol "FULL" $> TPFULL
    , tablePermissionsFor
    ]

defTable :: Parser Define
defTable = label "defTable" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "TABLE"
  tn <- tableName
  mDrop <- optional $ caseInsensitiveSymbol "DROP" $> DROP
  st <- optional $ choice $ map try
    [ caseInsensitiveSymbol "SCHEMAFULL" $> SCHEMAFULL
    , caseInsensitiveSymbol "SCHEMALESS" $> SCHEMALESS
    ]
  as <- optional $ caseInsensitiveSymbol "AS" >> maybeBetweenParens selectE
  dur <- optional $ caseInsensitiveSymbol "CHANGEFEED" >> durationParser
  DefTable tn mDrop st as dur <$> optional tablePermissions

defEvent :: Parser Define
defEvent = label "defEvent" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "EVENT"
  en <- identifier
  _ <- caseInsensitiveSymbol "ON"
  _ <- optional $ caseInsensitiveSymbol "TABLE"
  tn <- tableName
  whenE <- optional $ caseInsensitiveSymbol "WHEN" >> exp
  _ <- caseInsensitiveSymbol "THEN"
  DefEvent (EventName en) tn whenE <$> exp

arrayDef :: Parser (DataType, Maybe Int64)
arrayDef = label "arrayDef" $ lexeme $ do
  dt <- dataType
  mLen <- optional $ lexeme (symbol ",") >> intParser
  return (dt, mLen)

arrayT :: Parser DataType
arrayT = label "arrayT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "array"
  ad <- optional $ (between (char '<') (char '>') arrayDef <|>
                    betweenParens arrayDef)
  return $ ArrayT ad

setT :: Parser DataType
setT = label "setT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "set"
  ad <- optional $ (between (char '<') (char '>') arrayDef <|>
                    betweenParens arrayDef)
  return $ SetT ad

recordT :: Parser DataType
recordT = label "recordT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "record"
  tns <- (between (char '<') (char '>') (sepBy tableName (lexeme $ char '|')) <|>
          betweenParens (sepBy tableName (lexeme $ char '|')))
  return $ RecordT tns

geometryType :: Parser GeometryType
geometryType = label "geometryType" $ lexeme $ choice $ map try
  [ caseInsensitiveSymbol "feature" $> Feature
  , caseInsensitiveSymbol "point" $> Point
  , caseInsensitiveSymbol "line" $> Line
  , caseInsensitiveSymbol "polygon" $> Polygon
  , caseInsensitiveSymbol "multipoint" $> Multipoint
  , caseInsensitiveSymbol "multiline" $> Multiline
  , caseInsensitiveSymbol "multipolygon" $> Multipolygon
  , caseInsensitiveSymbol "collection" $> Collection
  ]

geometryT :: Parser DataType
geometryT = label "geometryT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "geometry"
  gts <- (between (char '<') (char '>') (sepBy geometryType (lexeme $ char '|')) <|>
          betweenParens (sepBy geometryType (lexeme $ char '|')))
  return $ GeometryT gts

-- order matters here, more specific first, ie ** before *
dataTypeOperatorTable :: [[E.Operator Parser DataType]]
dataTypeOperatorTable = [ [ E.InfixL (symbol "|" $> OrT) ] ]

dataType :: Parser DataType
dataType = E.makeExprParser dataTypeTerm dataTypeOperatorTable

dataTypeTerm :: Parser DataType
dataTypeTerm = label "dataType" $ lexeme $ choice $ map try
  [ AnyT <$ caseInsensitiveSymbol "any"
  , OptionalT <$> (caseInsensitiveSymbol "option" >> (between (char '<') (char '>') dataType <|>
                                                      betweenParens dataType))
  , arrayT
  , setT
  , recordT
  , geometryT
  , caseInsensitiveSymbol "string" $> StringT
  , caseInsensitiveSymbol "bool" $> BoolT
  , caseInsensitiveSymbol "datetime" $> DateTimeT
  , caseInsensitiveSymbol "decimal" $> DecimalT
  , caseInsensitiveSymbol "duration" $> DurationT
  , caseInsensitiveSymbol "float" $> FloatT
  , caseInsensitiveSymbol "int" $> IntT
  , caseInsensitiveSymbol "number" $> NumberT
  , caseInsensitiveSymbol "object" $> ObjectT
  ]

fieldType :: Parser FieldType
fieldType = label "fieldType" $ lexeme $ do
  flex <- optional $ caseInsensitiveSymbol "FLEXIBLE" $> Flexible
  _ <- caseInsensitiveSymbol "TYPE"
  FieldType flex <$> dataType

defField :: Parser Define
defField = label "defField" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "FIELD"
  f <- field
  _ <- caseInsensitiveSymbol "ON"
  _ <- optional $ caseInsensitiveSymbol "TABLE"
  tn <- tableName
  ft <- optional fieldType
  defaultE <- optional $ caseInsensitiveSymbol "DEFAULT" >> exp
  valE <- optional $ caseInsensitiveSymbol "VALUE" >> exp
  assertE <- optional $ caseInsensitiveSymbol "ASSERT" >> exp
  tp <- optional tablePermissions
  return $ DefField f tn ft defaultE valE assertE tp

tokenizer :: Parser Tokenizer
tokenizer = label "tokenizer" $ lexeme $ choice
  [ caseInsensitiveSymbol "BLANK" $> BLANK
  , caseInsensitiveSymbol "CAMEL" $> CAMEL
  , caseInsensitiveSymbol "CLASS" $> CLASS
  , caseInsensitiveSymbol "PUNCT" $> PUNCT
  ]

edgengram :: Parser Filter
edgengram = label "edgengram" $ lexeme $ do
  _ <- caseInsensitiveSymbol "edgengram("
  mn <- intParser
  _ <- caseInsensitiveSymbol ","
  mx <- intParser
  _ <- caseInsensitiveSymbol ")"
  return $ Edgengram mn mx

snowball :: Parser Filter
snowball = label "snowball" $ lexeme $ do
  _ <- caseInsensitiveSymbol "snowball("
  ln <- identifier
  _ <- caseInsensitiveSymbol ")"
  return $ Snowball (LanguageName ln)

filterParser :: Parser Filter
filterParser = label "filter" $ lexeme $ choice
  [ caseInsensitiveSymbol "ASCII" $> ASCII
  , caseInsensitiveSymbol "UPPERCASE" $> Uppercase
  , caseInsensitiveSymbol "LOWERCASE" $> Lowercase
  , edgengram
  , snowball
  ]

defAnalyzer :: Parser Define
defAnalyzer = label "defAnalyzer" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "ANALYZER"
  an <- identifier
  toks <- optional $ caseInsensitiveSymbol "TOKENIZERS" >> sepBy tokenizer (lexeme $ char ',')
  filters <- optional $ caseInsensitiveSymbol "FILTERS" >> sepBy filterParser (lexeme $ char ',')
  return $ DefAnalyzer (AnalyzerName an) toks filters

bm25 :: Parser BM25
bm25 = label "bm25" $ lexeme $ do
  _ <- caseInsensitiveSymbol "BM25"
  ps <- optional $ do
    _ <- caseInsensitiveSymbol "("
    k1 <- floatParser
    _ <- caseInsensitiveSymbol ","
    b <- floatParser
    _ <- caseInsensitiveSymbol ")"
    return (k1,b)
  return $ BM25 ps

searchAnalyzer :: Parser SearchAnalyzer
searchAnalyzer = label "searchAnalyzer" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SEARCH"
  _ <- caseInsensitiveSymbol "ANALYZER"
  an <- identifier
  bm <- optional bm25
  hl <- optional $ caseInsensitiveSymbol "HIGHLIGHTS" $> HIGHLIGHTS
  return $ SearchAnalyzer (AnalyzerName an) bm hl

defIndex :: Parser Define
defIndex = label "defIndex" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "INDEX"
  indxN <- identifier
  _ <- caseInsensitiveSymbol "ON"
  _ <- optional $ caseInsensitiveSymbol "TABLE"
  tn <- tableName
  _ <- caseInsensitiveSymbol "FIELDS" <|> caseInsensitiveSymbol "COLUMNS"
  fs <- sepBy field (lexeme $ char ',')
  u <- optional
    $ (caseInsensitiveSymbol "UNIQUE" $> Left UNIQUE) <|> Right <$> searchAnalyzer
  return $ DefIndex (IndexName indxN) tn fs u

defineS :: Parser Statement
defineS = label "defineS" $ lexeme $
  DefineS <$> choice
  (map try
    [ defNamespace
    , defDatabase
    , defUser
    , defToken
    , defScope
    , defTable
    , defEvent
    , defField
    , defAnalyzer
    , defIndex
    ])

throwS :: Parser Statement
throwS = label "ThrowS" $ lexeme $ do
  _ <- caseInsensitiveSymbol "THROW"
  ThrowS <$> exp

killParam :: Parser KillParam
killParam = label "killParam" $ lexeme $ do
  choice $ map try
    [ KPUUID <$> quotedText
    , KPParam <$> param
    ]

killS :: Parser Statement
killS = label "killS" $ lexeme $ do
  _ <- caseInsensitiveSymbol "KILL"
  KillS <$> killParam

nsdbScope :: Parser NSDBScope
nsdbScope = label "nsdbScope" $ lexeme $ choice $ map try
  [ caseInsensitiveSymbol "NAMESPACE" $> NSDBScopeNS
  , caseInsensitiveSymbol "DATABASE" $> NSDBScopeDB
  ]

removeParam :: Parser Remove
removeParam = label "removeParam" $ lexeme $ do
  choice $ map try
    [ caseInsensitiveSymbol "NAMESPACE" >> (Namespace <$> identifier) <&> RMNS
    , caseInsensitiveSymbol "DATABASE" >> (Database <$> identifier) <&> RMDB
    , caseInsensitiveSymbol "USER" >> RMUser <$> (UserName <$> identifier) <*> (caseInsensitiveSymbol "ON" >> userScope)
    , caseInsensitiveSymbol "LOGIN" >> RMLogin <$> (LoginName <$> identifier) <*> (caseInsensitiveSymbol "ON" >> nsdbScope)
    , caseInsensitiveSymbol "TOKEN" >> RMToken <$> (TokenName <$> identifier) <*> (caseInsensitiveSymbol "ON" >> nsdbScope)
    , caseInsensitiveSymbol "SCOPE" >> RMScope <$> (ScopeName <$> identifier)
    , caseInsensitiveSymbol "TABLE" >> RMTable <$> tableName
    , caseInsensitiveSymbol "EVENT" >> RMEvent <$> (EventName <$> identifier)
      <*> (do
              _ <- caseInsensitiveSymbol "ON"
              _ <- optional $ caseInsensitiveSymbol "TABLE"
              tableName)
    , caseInsensitiveSymbol "FUNCTION" >> RMFN <$> fnName
    , caseInsensitiveSymbol "FIELD" >> RMField <$> (FieldName <$> identifier)
      <*> (do
              _ <- caseInsensitiveSymbol "ON"
              _ <- optional $ caseInsensitiveSymbol "TABLE"
              tableName)
    , caseInsensitiveSymbol "INDEX" >> RMIndex . IndexName <$> identifier
      <*> (do
              _ <- caseInsensitiveSymbol "ON"
              _ <- optional $ caseInsensitiveSymbol "TABLE"
              tableName)
    , caseInsensitiveSymbol "PARAM" >> RMParam <$> (char '$' >> (ParamName <$> identifier))
    ]

removeS :: Parser Statement
removeS = label "removeS" $ lexeme $ do
  _ <- caseInsensitiveSymbol "REMOVE"
  RemoveS <$> removeParam

sleepS :: Parser Statement
sleepS = label "sleepS" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SLEEP"
  SleepS <$> durationParser

statement :: Parser Statement
statement =
  sc >> lexeme (choice $ map try
                [ useS
                , letS
                , caseInsensitiveSymbol "BEGIN"
                  >> optional (caseInsensitiveSymbol "TRANSACTION")
                  $> BeginS
                , caseInsensitiveSymbol "CANCEL"
                  >> optional (caseInsensitiveSymbol "TRANSACTION")
                  $> CancelS
                , caseInsensitiveSymbol "COMMIT"
                  >> optional (caseInsensitiveSymbol "TRANSACTION")
                  $> CommitS
                , caseInsensitiveSymbol "BREAK" $> BreakS
                , caseInsensitiveSymbol "CONTINUNE" $> ContinueS
                , forS
                , defineS
                , throwS
                , killS
                , removeS
                , sleepS
                ])

expLine :: Parser SurQLLine
expLine = lexeme $ do
  l <- ExpLine <$> exp
  _ <- (void $ symbol ";") <|> (void $ lookAhead (symbol "}")) <|> lookAhead eof
  return l

statementLine :: Parser SurQLLine
statementLine = lexeme $ do
  l <- StatementLine <$> statement
  _ <- (void $ symbol ";") <|> (void $ lookAhead (symbol "}")) <|> lookAhead eof
  return l

surQLLine :: Parser SurQLLine
surQLLine = lexeme $ choice $ map try
  [ statementLine
  , expLine
  ]

block :: Parser Block
block = sc >> lexeme
  (do
    ls <- some surQLLine
    when (null ls) (fail "At least one SQL line expected!")
    return $ Block ls
  )

-- | strict version of block, makes sure the string ends after the blocks
blockStrict :: Parser Block
blockStrict = sc >> lexeme
  (do
    bl <- block
    eof
    return bl)

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
import           Data.Char                      ( isAlpha, isAlphaNum )
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
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveSymbol :: String -> Parser String
caseInsensitiveSymbol s = label s $ lexeme $ L.symbol' sc s

betweenParens :: Parser a -> Parser a
betweenParens = between (lexeme $ char '(') (lexeme $ char ')')

maybeBetweenParens :: Parser a -> Parser a
maybeBetweenParens p = p <|> betweenParens p

-- | Type Parsers

simpleType :: Parser TypeDef
simpleType = do
  prefix <- try $ lexeme (parseCapitalWord <|> symbol "()")
  postfix <- many
    $ try
    $ lexeme
    $ between (char '(') (char ')') simpleType <|> typeCon
  return $ T prefix postfix

parseType :: Parser TypeDef
parseType = lexeme $ maybeBetweenParens simpleType

-- | Input
input :: Parser Input
input = label "Input" $ lexeme $ maybeBetweenParens $ do
  _ <- char '%'
  i <- lexeme intParser
  _ <- lexeme (symbol "::")
  Input i <$> parseType

-- | Literals

noneL :: Parser Literal
noneL = label "NONE" $ lexeme $ symbol "NONE" $> NoneL

nullL :: Parser Literal
nullL = label "NULL" $ lexeme $ symbol "NULL" $> NoneL

boolL :: Parser Literal
boolL = label "Bool" $ lexeme $ BoolL False <$ symbol "False" <|> BoolL False <$ symbol "True"

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
      Just r -> return r
      _      -> fail "Invalid Int64"

floatParser :: Parser Float
floatParser = label "Float" $ do
  s <- some numberChar <> symbol "." <> some numberChar
  let mr = readMay s :: Maybe Float
    in
    case mr of
      Just r -> return r
      _      -> fail "Invalid Float"

int64L :: Parser Literal
int64L = label "Int64L" $ lexeme $ Int64L <$> L.signed sc intParser

floatL :: Parser Literal
floatL = label "FloatL" $ lexeme $ FloatL <$> L.signed sc floatParser

dateTimeL :: Parser Literal
dateTimeL = label "UTCTime" $ lexeme $ do
  s <- someTill asciiChar (char 'Z')
  let mUTC = parseISO8601 $ s <> "Z"
  case mUTC of
    Just utc -> return $ DateTimeL utc
    _        -> fail "Invalid ISO8601 DateTime"

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

literalInput :: Parser Literal
literalInput = label "LiteralInput" $ lexeme $ LiteralInput <$> input

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

idInput :: Parser ID
idInput = label "idInput" $ lexeme $ IDInput <$> input

id_ :: Parser ID
id_ = label "ID" $ choice $ map try
  [ randomID
  , numID
  , textID
  , objID
  , tupID
  , idInput
  ]

normalRecordID :: Parser RecordID
normalRecordID = label "normalRecordID" $ lexeme $ do
  tn <- tableName
  _ <- symbol ":"
  RecordID tn <$> id_

recordIDInput :: Parser RecordID
recordIDInput = label "recordIDInput" $ lexeme $ RecordIDInput <$> input

recordID :: Parser RecordID
recordID = label "RecordID" $ lexeme $ choice
  [ normalRecordID
  , recordIDInput
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
  , recordIDRangeL
  , recordIDL
  , objectL
  , arrayL
  , futureL
  , literalInput
  ]

simpleField :: Parser Field
simpleField = label "SimpleField" $ lexeme $ do
  initial <- satisfy isAlpha
  rest <- optional $ some $ satisfy isValidIdentifierChar
  return $ SimpleField $ pack $ initial : fromMaybe "" rest

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
  f <- simpleField
  FilteredField f <$> where_

fieldInput :: Parser Field
fieldInput = label "FieldInput" $ FieldInput <$> input

fieldOperatorTable :: [[E.Operator Parser Field]]
fieldOperatorTable = [ [ E.Postfix indexedField
                       , E.InfixL (symbol "." $> CompositeField)
                       ] ]

field :: Parser Field
field = E.makeExprParser fieldTerm fieldOperatorTable

fieldTerm :: Parser Field
fieldTerm = lexeme $ choice $ map try
  [ filteredField
  , simpleField
  , fieldInput
  ]

isValidIdentifierChar :: Char -> Bool
isValidIdentifierChar c = isAlphaNum c || c == '_'

fnName :: Parser FNName
fnName = label "FNName" $ lexeme $ do
  parts <- sepBy partParser (symbol "::")
  if null parts
    then fail "Invalid function name."
    else
      return $ FNName $ pack $ intercalate "::" parts
  where
    partParser = do
      initial <- satisfy isAlpha
      rest <- optional $ some $ satisfy isValidIdentifierChar
      return $ initial : fromMaybe "" rest

appE :: Parser Exp
appE = label "AppE" $ lexeme $ do
  fnn <- fnName
  exps <- betweenParens $ sepBy exp (lexeme $ char ',')
  return $ AppE fnn exps

litE :: Parser Exp
litE = label "LitE" $ lexeme $ LitE <$> literal

constE :: Parser Exp
constE = label "ConstE" $ lexeme $ ConstE . pack <$> some alphaNumChar

param :: Parser Param
param = label "Param" $ lexeme $ do
  _ <- char '$'
  p <- pack <$> some alphaNumChar
  return $ Param p

paramE :: Parser Exp
paramE = label "ParamE" $ lexeme $ ParamE <$> param

inputE :: Parser Exp
inputE = label "InputE" $ lexeme $ do
  InputE <$> input

ifThenE :: Parser Exp
ifThenE = label "IfThenE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "IF"
  e1 <- exp
  _ <- caseInsensitiveSymbol "THEN"
  IfThenE e1 <$> exp

ifThenElseE :: Parser Exp
ifThenElseE = label "IfThenElseE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "IF"
  e1 <- exp
  _ <- caseInsensitiveSymbol "THEN"
  e2 <- exp
  _ <- caseInsensitiveSymbol "ELSE"
  IfThenElseE e1 e2 <$> exp

parseCapitalWord :: Parser String
parseCapitalWord = lexeme $ do
  initialLetter <- upperChar
  rest <- some alphaNumChar
  return $ initialLetter : rest

typeCon :: Parser TypeDef
typeCon = do
  w <- try $ lexeme parseCapitalWord
  return $ T w []

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

wildCardSelector :: Parser Selector
wildCardSelector = label "wildCardSelector" $ lexeme $ symbol "*" $> WildCardSelector

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
  , wildCardSelector
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
    indexes = INDEX <$> sepBy (pack <$> some alphaNumChar) (char ',')

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
  <|> OrderTypeInput <$> input

orderDirection :: Parser OrderDirection
orderDirection = label "OrderDirection" $ lexeme $ do
  caseInsensitiveSymbol "ASC" $> ASC
  <|> caseInsensitiveSymbol "DESC" $> DESC
  <|> OrderDirectionInput <$> input

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
  LIMIT <$> intParser <|> LIMITInput <$> input

start :: Parser START
start = label "START" $ lexeme $ do
  _ <- caseInsensitiveSymbol "START"
  _ <- optional $ caseInsensitiveSymbol "AT"
  START <$> intParser <|> STARTInput <$> input

fetch :: Parser FETCH
fetch = label "FETCH" $ lexeme $ do
  _ <- caseInsensitiveSymbol "FETCH"
  FETCH <$> sepBy field (char ',')

timeout :: Parser TIMEOUT
timeout = label "TIMEOUT" $ lexeme $ do
  _ <- caseInsensitiveSymbol "TIMEOUT"
  choice
    [ TIMEOUT <$> durationParser
    , TIMEOUTInput <$> input
    ]

parallel :: Parser PARALLEL
parallel = label "PARALLEL" $ lexeme $ caseInsensitiveSymbol "PARALLEL" $> PARALLEL

explain :: Parser EXPLAIN
explain = label "EXPLAIN" $ lexeme $ do
  _ <- caseInsensitiveSymbol "EXPLAIN"
  mFull <- optional $ caseInsensitiveSymbol "FULL"
  return $ if isJust mFull then EXPLAINFULL else EXPLAIN

tableName :: Parser TableName
tableName = label "TableName" $ lexeme
  $ TableName <$> (pack <$> some alphaNumChar)
  <|> TableNameInput <$> input

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
  mOnDuplicate <- optional onDuplicate
  return $ InsertValues fs vs mOnDuplicate
  where
    tupleParser = lexeme $ betweenParens $ sepBy exp (lexeme $ char ',')

insertVal :: Parser InsertVal
insertVal = label "insertVal" $ lexeme $ choice
  [ insertObject
  , insertValues
  ]

insertE :: Parser Exp
insertE = label "insertE" $ lexeme $ do
  _ <- caseInsensitiveSymbol "INSERT"
  mIgnore <- optional $ caseInsensitiveSymbol "IGNORE" $> IGNORE
  _ <- caseInsensitiveSymbol "INTO"
  tn <- tableName
  InsertE mIgnore tn <$> insertVal

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
  ]

createObject :: Parser CreateVal
createObject = label "createObject" $ lexeme $ do
  _ <- lexeme $ caseInsensitiveSymbol "CONTENT"
  CreateObject <$> object_

createValues :: Parser CreateVal
createValues = label "createValues" $ lexeme $ do
  _ <- lexeme $ caseInsensitiveSymbol "SET"
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
  _ <- lexeme $ caseInsensitiveSymbol "CONTENT"
  UpdateObject <$> object_

updateMerge :: Parser UpdateVal
updateMerge = label "updateMerge" $ lexeme $ do
  _ <- lexeme $ caseInsensitiveSymbol "MERGE"
  UpdateMerge <$> object_

updatePatch :: Parser UpdateVal
updatePatch = label "updatePatch" $ lexeme $ do
  _ <- lexeme $ caseInsensitiveSymbol "PATCH"
  UpdatePatch <$> object_

updateValues :: Parser UpdateVal
updateValues = label "updateValues" $ lexeme $ do
  _ <- lexeme $ caseInsensitiveSymbol "SET"
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
edgeSelectorE = label "selectorE" $ lexeme $ do
  mInitialField <- optional field
  edges <- some edge
  if null edges
    then fail "Invalid Edge!"
    else return $ EdgeSelectorE mInitialField edges

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
                  , E.InfixN (symbol "IN" $> OPE IN)
                  , E.InfixN (symbol "NOTIN" $> OPE NOTIN)
                  , E.InfixN (symbol "CONTAINSNOT" $> OPE CONTAINSNOT)
                  , E.InfixN (symbol "CONTAINSALL" $> OPE CONTAINSALL)
                  , E.InfixN (symbol "CONTAINSANY" $> OPE CONTAINSANY)
                  , E.InfixN (symbol "CONTAINSNONE" $> OPE CONTAINSNONE)
                  , E.InfixN (symbol "CONTAINS" $> OPE CONTAINS)
                  , E.InfixN (symbol "INSIDE" $> OPE INSIDE)
                  , E.InfixN (symbol "NOTINSIDE" $> OPE NOTINSIDE)
                  , E.InfixN (symbol "ALLINSIDE" $> OPE ALLINSIDE)
                  , E.InfixN (symbol "ANYINSIDE" $> OPE ANYINSIDE)
                  , E.InfixN (symbol "NONEINSIDE" $> OPE NONEINSIDE)
                  , E.InfixN (symbol "OUTSIDE" $> OPE OUTSIDE)
                  , E.InfixN (symbol "INTERSECTS" $> OPE INTERSECTS)
                  , E.InfixN (symbol "@@" $> OPE (:@@))
                  ]
                , [ E.InfixN (symbol "&&" $> OPE (:&&))
                  , E.InfixN (symbol "||" $> OPE (:||))]
                ]

exp :: Parser Exp
exp = E.makeExprParser term operatorTable

term :: Parser Exp
term = sc
  >> lexeme (choice $ map try
              [ paramE
              , inputE
              , ifThenE
              , ifThenElseE
              , selectE
              , insertE
              , createE
              , updateE
              , relateE
              , deleteE
              , appE
              , litE
              , constE
              , edgeSelectorE
              , betweenParens exp
              ])

useNSDB :: Parser Statement
useNSDB = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "NS"
  ns <- pack <$> some alphaNumChar
  _ <- caseInsensitiveSymbol "DB"
  db <- pack <$> some alphaNumChar
  return $ UseS $ USE ns db

useNS :: Parser Statement
useNS = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "NS"
  ns <- pack <$> some alphaNumChar
  return $ UseS $ USE_NS ns

useDB :: Parser Statement
useDB = label "useNSDB" $ lexeme $ do
  _ <- caseInsensitiveSymbol "USE"
  _ <- caseInsensitiveSymbol "DB"
  db <- pack <$> some alphaNumChar
  return $ UseS $ USE_DB db

useS :: Parser Statement
useS = label "useS" $ lexeme $ choice $ map try
  [ useNSDB
  , useNS
  , useDB
  ]

letS :: Parser Statement
letS = label "letS" $ lexeme $ do
  p <- param
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
  DefNamespace <$> (pack <$> some alphaNumChar)

defDatabase :: Parser Define
defDatabase = label "defDatabase" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "DATABASE"
  DefDatabase <$> (pack <$> some alphaNumChar)

userScope :: Parser UserScope
userScope = label "userScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "ON"
  choice [ caseInsensitiveSymbol "OWNER" $> USROOT
         , caseInsensitiveSymbol "NAMESPACE" $> USNS
         , caseInsensitiveSymbol "DATABASE" $> USDB
         , caseInsensitiveSymbol "SCOPE" >> (pack <$> some alphaNumChar) <&> USScope
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
  un <- pack <$> some alphaNumChar
  scope <- userScope
  pass <- optional userPass
  role <- optional userRole
  return $ DefUser un scope pass role

tokenScope :: Parser TokenScope
tokenScope = label "tokenScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "ON"
  choice [ caseInsensitiveSymbol "NAMESPACE" $> TSNS
         , caseInsensitiveSymbol "DATABASE" $> TSDB
         , caseInsensitiveSymbol "SCOPE" >> (pack <$> some alphaNumChar) <&> TSScope
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
  tn <- pack <$> some alphaNumChar
  scope <- optional tokenScope
  _ <- caseInsensitiveSymbol "TYPE"
  t <- tokenType
  DefToken tn scope t <$> (pack <$> some alphaNumChar)

defScope :: Parser Define
defScope = label "defScope" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "SCOPE"
  sn <- pack <$> some alphaNumChar
  _ <- caseInsensitiveSymbol "SESSION"
  dur <- durationParser
  _ <- caseInsensitiveSymbol "SIGNUP"
  signUpExp <- exp
  _ <- caseInsensitiveSymbol "SIGNIN"
  DefScope sn dur signUpExp <$> exp

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
  as <- optional $ caseInsensitiveSymbol "AS" >> selectE
  dur <- optional $ caseInsensitiveSymbol "CHANGEFEED" >> durationParser
  DefTable tn mDrop st as dur <$> optional tablePermissions

defEvent :: Parser Define
defEvent = label "defEvent" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "EVENT"
  en <- pack <$> some alphaNumChar
  _ <- caseInsensitiveSymbol "ON"
  _ <- optional $ caseInsensitiveSymbol "TABLE"
  tn <- tableName
  whenE <- optional $ caseInsensitiveSymbol "WHEN" >> exp
  _ <- caseInsensitiveSymbol "THEN"
  DefEvent en tn whenE <$> exp

fieldType :: Parser FieldType
fieldType = label "fieldType" $ lexeme $ do
  flex <- optional $ caseInsensitiveSymbol "FLEXIBLE" $> Flexible
  _ <- caseInsensitiveSymbol "TYPE"
  mOptional <- optional $ caseInsensitiveSymbol "option<" $> Optional
  t <- pack <$> some alphaNumChar
  _ <- if isJust mOptional then return () else void $ caseInsensitiveSymbol ">"
  return $ FieldType flex mOptional t

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
  ln <- pack <$> some alphaNumChar
  _ <- caseInsensitiveSymbol ")"
  return $ Snowball ln

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
  an <- pack <$> some alphaNumChar
  toks <- optional $ caseInsensitiveSymbol "TOKENIZERS" >> sepBy tokenizer (lexeme $ char ',')
  filters <- optional $ caseInsensitiveSymbol "FILTERS" >> sepBy filterParser (lexeme $ char ',')
  return $ DefAnalyzer an toks filters

bm25 :: Parser BM25
bm25 = label "bm25" $ lexeme $ do
  _ <- caseInsensitiveSymbol "BM25"
  ps <- optional $ do
    k1 <- floatParser
    _ <- caseInsensitiveSymbol ","
    b <- floatParser
    return (k1,b)
  return $ BM25 ps

searchAnalyzer :: Parser SearchAnalyzer
searchAnalyzer = label "searchAnalyzer" $ lexeme $ do
  _ <- caseInsensitiveSymbol "SEARCH"
  _ <- caseInsensitiveSymbol "ANALYZER"
  an <- pack <$> some alphaNumChar
  bm <- optional bm25
  hl <- optional $ caseInsensitiveSymbol "HIGHLIGHTS" $> HIGHLIGHTS
  return $ SearchAnalyzer an bm hl

defIndex :: Parser Define
defIndex = label "defIndex" $ lexeme $ do
  _ <- caseInsensitiveSymbol "DEFINE"
  _ <- caseInsensitiveSymbol "INDEX"
  indxN <- pack <$> some alphaNumChar
  _ <- caseInsensitiveSymbol "ON"
  _ <- optional $ caseInsensitiveSymbol "TABLE"
  tn <- tableName
  _ <- caseInsensitiveSymbol "FIELDS" <|> caseInsensitiveSymbol "COLUMNS"
  fs <- sepBy field (lexeme $ char ',')
  u <- optional
    $ (caseInsensitiveSymbol "UNIQUE" $> Left UNIQUE) <|> Right <$> searchAnalyzer
  return $ DefIndex indxN tn fs u

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

statement :: Parser Statement
statement = lexeme $ choice $ map try
  [ useS
  , letS
  , caseInsensitiveSymbol "BEGIN" $> BeginS
  , caseInsensitiveSymbol "CANCEL" $> CancelS
  , caseInsensitiveSymbol "COMMIT" $> CommitS
  , caseInsensitiveSymbol "BREAK" $> BreakS
  , caseInsensitiveSymbol "CONTINUNE" $> ContinueS
  , forS
  , defineS
  ]

expLine :: Parser SurQLLine
expLine = lexeme $ do
  l <- ExpLine <$> exp
  _ <- symbol ";"
  return l

statementLine :: Parser SurQLLine
statementLine = lexeme $ do
  l <- StatementLine <$> statement
  _ <- symbol ";"
  return l

surQLLine :: Parser SurQLLine
surQLLine = lexeme $ choice $ map try
  [ statementLine
  , expLine
  ]

block :: Parser Block
block = lexeme $ do
  bl <- Block <$> some surQLLine
  eof
  return bl

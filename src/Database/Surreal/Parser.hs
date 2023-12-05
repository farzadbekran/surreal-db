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
import           Text.Read                      ( read )

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
betweenParens = between (char '(') (char ')')

maybeBetweenParens :: Parser a -> Parser a
maybeBetweenParens p = p <|> betweenParens p

-- | Type Parsers

simpleType :: Parser TypeDef
simpleType = do
  prefix <- try $ lexeme parseCapitalWord
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

textL :: Parser Literal
textL = label "Text" $ lexeme $ TextL . pack <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

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

durationL :: Parser Literal
durationL = label "Duration" $ lexeme $ do
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
  return $ DurationL duration

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
  space1 <|> eof
  return $ NumID i

objID :: Parser ID
objID = label "objID" $ lexeme $ ObjID <$> object_

objectField :: Parser (Field, Exp)
objectField = label "objectField" $ lexeme $ do
  f <- lexeme $ between (char '"') (char '"') field
  _ <- lexeme $ symbol ":"
  e <- exp
  return (f,e)

object_ :: Parser Object
object_ = label "Object" $ lexeme $ do
  fields <- between (char '{') (char '}') $ sepBy objectField (lexeme $ char ',')
  return $ Object fields

tupID :: Parser ID
tupID = label "tupID" $ lexeme
  $ TupID <$> between (char '[') (char ']')
  (sepBy exp (lexeme $ char ','))

randomID :: Parser ID
randomID = label "randomID" $ lexeme $ choice
  [ caseInsensitiveSymbol "rand()" $> RandomID (FNName "rand")
  , caseInsensitiveSymbol "ulid()" $> RandomID (FNName "ulid")
  , caseInsensitiveSymbol "uuid()" $> RandomID (FNName "uuid")
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

paramE :: Parser Exp
paramE = label "ParamE" $ lexeme $ do
  _ <- char '$'
  p <- pack <$> some alphaNumChar
  return $ ParamE (Param p)

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
  read <$> some numberChar <&> TIMEOUT

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
insertObject = label "insertObject" $ lexeme $ InsertObject <$> object_

insertValues :: Parser InsertVal
insertValues = label "insertValues" $ lexeme $ do
  fs <- lexeme $ betweenParens $ sepBy field (char ',')
  _ <- caseInsensitiveSymbol "VALUES"
  vs <- sepBy tupleParser (char ',')
  mOnDuplicate <- optional onDuplicate
  return $ InsertValues fs vs mOnDuplicate
  where
    tupleParser = lexeme $ betweenParens $ sepBy exp (char ',')

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

typedExp :: Parser (Exp -> Exp)
typedExp = do
  _ <- lexeme $ symbol "::"
  t <- parseType
  return (`TypedE` t)

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
              , appE
              , litE
              , constE
              , betweenParens exp
              ])

-- TODO: finish this
statement :: Parser Statement
statement = lexeme $ choice $ map try
  []

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

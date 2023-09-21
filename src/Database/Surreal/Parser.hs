{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Database.Surreal.Parser where

import           ClassyPrelude                  hiding ( bool, exp, group,
                                                  index, some, timeout, try )
import qualified Control.Monad.Combinators.Expr as E
import           Control.Monad.Fail             ( MonadFail (..) )
import qualified Data.Char                      as C
import           Data.Foldable                  ( foldl )
import           Data.Time.ISO8601              ( parseISO8601 )
import           Data.Void
import           Database.Surreal.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Read                      ( read )

type Parser = Parsec Void String

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
                           "y"  -> dur { y = y dur + i}
                           "w"  -> dur { w = w dur + i}
                           "d"  -> dur { d = d dur + i}
                           "h"  -> dur { h = h dur + i}
                           "m"  -> dur { m = m dur + i}
                           "s"  -> dur { s = s dur + i}
                           "ms" -> dur { ms = ms dur + i}
                           "us" -> dur { us = us dur + i}
                           "ns" -> dur { ns = ns dur + i}
                           _    -> dur
                       )
                 defaultDuration parts
  return $ DurationL duration

-- | TODO: add missing types like object
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
  ]

field :: Parser Field
field = label "Field" $ lexeme $ some alphaNumChar <&> Field . pack

fnName :: Parser FNName
fnName = label "FNName" $ lexeme $ (some (satisfy C.isAlpha) <> some alphaNumChar) <&> FNName . pack

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

fieldSelector :: Parser Selector
fieldSelector = label "FieldSelector" $ FieldSelector <$> field

expSelector :: Parser Selector
expSelector = label "ExpSelector" $ lexeme $ do
  e <- exp
  _ <- caseInsensitiveSymbol "AS"
  ExpSelector e <$> field

fieldSelectorAs :: Parser Selector
fieldSelectorAs = label "FieldSelectorAs" $ lexeme $ do
  f <- field
  _ <- caseInsensitiveSymbol "AS"
  FieldSelectorAs f <$> field

selector :: Parser Selector
selector = lexeme $ choice $ map try
  [ fieldSelectorAs
  , fieldSelector
  , expSelector
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

orderDirection :: Parser OrderDirection
orderDirection = label "OrderDirection" $ lexeme $ do
  caseInsensitiveSymbol "ASC" $> ASC
  <|> caseInsensitiveSymbol "DESC" $> DESC

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
  read <$> some numberChar <&> LIMIT

start :: Parser START
start = label "START" $ lexeme $ do
  _ <- caseInsensitiveSymbol "START"
  _ <- optional $ caseInsensitiveSymbol "AT"
  read <$> some numberChar <&> START

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
  -- return $ SelectE mValue sels Nothing from_ Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- order matters here, more specific first, ie ** before *
operatorTable :: [[E.Operator Parser Exp]]
operatorTable = [ [ E.InfixN (symbol "&&" $> OPE (:&&))
                  , E.InfixN (symbol "**" $> OPE (:**))
                  , E.InfixN (symbol "||" $> OPE (:||))
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
                  ] ]

exp :: Parser Exp
exp = E.makeExprParser term operatorTable

term :: Parser Exp
term = lexeme $ choice
  [ paramE
  , ifThenE
  , ifThenElseE
  , selectE
  , try appE
  , try litE
  , try constE
  , betweenParens exp
  ]

-- object :: Parser (Object a)
-- object = label "Object" $ do
--   attribs <- label "Object Attribute" $ label <$> char ':' <*> exp

-- objectL :: Parser (Literal (Object a))
-- objectL = label "Object Literal" $ object <&> ObjectL

-- atom :: Parser (Literal a)
-- atom = choice
--   [ BoolL <$> bool
--   ]

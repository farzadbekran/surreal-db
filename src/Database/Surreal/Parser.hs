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
import qualified Data.Char                      as C
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

int64L :: Parser Literal
int64L = label "Int64" $ lexeme $ read <$> some numberChar <&> Int64L

decimalL :: Parser Literal
decimalL = label "Decimal" $ lexeme $ read <$> some numberChar <&> DecimalL

floatL :: Parser Literal
floatL = label "Float" $ lexeme $ read <$> some numberChar <&> FloatL

-- | TODO: this needs fixing
dateTimeL :: Parser Literal
dateTimeL = label "UTCTime" $ lexeme $ read <$> some numberChar <&> DateTimeL

-- | TODO: this needs fixing
durationL :: Parser Literal
durationL = label "Duration" $ lexeme $ some alphaNumChar <&> DurationL . Duration . pack

literal :: Parser Literal
literal = lexeme $ maybeBetweenParens $ choice
  [ noneL
  , nullL
  , boolL
  , textL
  , int64L
  , decimalL
  , floatL
  , dateTimeL
  , durationL
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
  FieldSelectorAs f <$> field

selector :: Parser Selector
selector = label "Selector" $ lexeme $ choice
  [ fieldSelector
  , expSelector
  , fieldSelectorAs
  ]

selectors :: Parser Selectors
selectors = label "[Selector]" $ lexeme $ Selectors <$> sepBy selector (lexeme $ char ',')

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

operatorTable :: [[E.Operator Parser Exp]]
operatorTable = [ [ E.InfixL (symbol "*" $> OPE (:*))
                  , E.InfixL (symbol ">" $> OPE (:>))
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

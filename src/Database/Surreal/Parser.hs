{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -Wno-deriving-defaults #-}

module Database.Surreal.Parser where

import           ClassyPrelude              hiding ( bool, some )
import           Data.Decimal               ( Decimal )
import qualified Data.Row                   as R
import           Data.Void
import           Database.Surreal.AST
import           GHC.TypeLits
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read                  ( read )
import Data.Proxy

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Eats whitespace after the parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse the exact String given
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Literals

noneL :: Parser (Literal ())
noneL = label "NONE" $ symbol "NONE" $> NoneL

nullL :: Parser (Literal ())
nullL = label "NULL" $ symbol "NULL" $> NoneL

boolL :: Parser (Literal Bool)
boolL = label "Bool" $ BoolL False <$ symbol "False" <|> BoolL False <$ symbol "True"

textL :: Parser (Literal Text)
textL = label "Text" $ TextL . pack <$> between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

int64L :: Parser (Literal Int64)
int64L = label "Int64" $ read <$> some numberChar <&> Int64L

decimalL :: Parser (Literal Decimal)
decimalL = label "Decimal" $ read <$> some numberChar <&> DecimalL

floatL :: Parser (Literal Float)
floatL = label "Float" $ read <$> some numberChar <&> FloatL

dateTimeL :: Parser (Literal UTCTime)
dateTimeL = label "UTCTime" $ read <$> some numberChar <&> DateTimeL

durationL :: Parser (Literal Duration)
durationL = label "Duration" $ some alphaNumChar <&> DurationL . Duration . pack

rowLabel :: Parser SomeSymbol
rowLabel = do
  _ <- char '#'
  symStr <- some alphaNumChar
  return $ someSymbolVal symStr

-- object :: Parser (Object a)
-- object = label "Object" $ do
--   attribs <- label "Object Attribute" $ label <$> char ':' <*> exp

-- objectL :: Parser (Literal (Object a))
-- objectL = label "Object Literal" $ object <&> ObjectL

-- atom :: Parser (Literal a)
-- atom = choice
--   [ BoolL <$> bool
--   ]

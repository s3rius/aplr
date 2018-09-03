module Parser
    ( parseAPLR
    ) where

import Types

import Data.Ratio
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "⍝"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol    = L.symbol sc

simpleInteger :: Parser Integer
simpleInteger = lexeme L.decimal

integer = (try simpleInteger) <|> parens simpleInteger

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

double :: Parser Double
double = lexeme L.float

rational :: Parser Rational
rational = do
  numerator <- integer
  void (symbol "%")
  denominator <- integer
  return $ numerator % denominator

signedRational :: Parser Rational
signedRational = L.signed sc rational

signedDouble :: Parser Double
signedDouble = L.signed sc double

aplr_integer :: Parser APLR_Value
aplr_integer = do
  val <- signedInteger
  return $ APLR_Int val

aplr_double :: Parser APLR_Value
aplr_double = do
  val <- signedDouble
  return $ APLR_Float val

aplr_rational :: Parser APLR_Value
aplr_rational = do
  val <- signedRational
  return $ APLR_Rational val

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

aplr_value :: Parser APLR_Value
aplr_value = (try aplr_double) <|> (try aplr_rational) <|> aplr_integer <|> parens aplr_value

aplr_expression :: Parser APLR_Syntax
aplr_expression = do
  val <- aplr_value
  return $ APLR_Syntax_value val

parseAPLR = parse aplr_expression ""

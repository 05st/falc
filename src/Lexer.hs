{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances#-}

module Lexer where

import Data.Text (Text, singleton)
import Data.Void
import Data.Char
import Data.Functor.Identity

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- import Error.Diagnose
-- import Error.Diagnose.Compat.Megaparsec

type Parser = P.ParsecT Void Text Identity
    
spaces :: Parser ()
spaces = L.space P.space1 P.empty P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

decimal :: Parser Integer
decimal = lexeme L.decimal

octal :: Parser Integer
octal = lexeme (P.char '0' *> P.char 'o' *> L.octal)

hexadecimal :: Parser Integer
hexadecimal = lexeme (P.char '0' *> P.char 'x' *> L.hexadecimal)

binary :: Parser Integer
binary = lexeme (P.char '0' *> P.char 'b' *> L.binary)

float :: Parser Double
float = lexeme L.float

identifier :: Parser Text
identifier = lexeme (mappend <$> (singleton <$> P.letterChar) <*> P.takeWhileP (Just "alphanumeric character") isAlphaNum)

parens :: Parser a -> Parser a
parens = P.between (lexeme (P.char '(')) (lexeme (P.char ')'))

comma :: Parser Char
comma = lexeme (P.char ',')
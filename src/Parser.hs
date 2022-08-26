{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (Parser.parse) where

import Data.Text (Text)
import Data.Functor.Identity
import Data.Bifunctor (first)

import Text.Megaparsec

import Control.Monad.Combinators.Expr

import Lexer
import Expr
import Error

pExpression :: Parser Expr
pExpression = makeExprParser pTerm operTable <?> "expression"
    where
        operTable =
            [ [ postfix Fac ]
            , [ prefix Sub
              , prefix Add ]
            , [ binary InfixR Pow ]
            , [ binary InfixL Mul
              , binary InfixL Div ]
            , [ binary InfixL Add
              , binary InfixL Sub ] ]

        binary assoc oper = assoc (EBinOp oper <$ operParser oper)
        prefix oper = Prefix (EUnaOp oper <$ operParser oper)
        postfix oper = Postfix (EUnaOp oper <$ operParser oper)

        operParser = symbol . operSymbol
        operSymbol = \case
            Add -> "+"
            Sub -> "-"
            Mul -> "*"
            Div -> "/"
            Pow -> "^"
            Fac -> "!"

pTerm :: Parser Expr
pTerm = parens pExpression <|> (EVar <$> identifier) <|> (ELit <$> pLiteral) <?> "term"

pLiteral :: Parser Literal
pLiteral = (try (LFloat <$> float) <|> LInt <$> integer) <?> "literal"
    where
        integer = try octal <|> try binary <|> try hexadecimal <|> decimal

parse :: Text -> Either Error Expr
parse = first ParserError . runIdentity . runParserT pExpression "falc"
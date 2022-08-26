module Expr where

import Data.Text (Text)

data Oper
    = Add
    | Sub
    | Mul
    | Div
    | Pow
    | Fac
    deriving (Show)

data Literal
    = LInt Integer
    | LFloat Double
    deriving (Show)

data Expr
    = ELit Literal
    | EVar Text
    | EBinOp Oper Expr Expr
    | EUnaOp Oper Expr
    deriving (Show)
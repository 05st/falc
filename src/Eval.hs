{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Eval where

import Data.Bifunctor (first)

import Control.Monad.Except

import Expr
import Error

type Eval a = Except String a

evaluateExpr :: Expr -> Eval Double
evaluateExpr = \case
    ELit lit -> return (evaluateLiteral lit)
    EVar var ->
        case var of
            "pi" -> return pi
            "e" -> return (exp 1)
            other -> throwError ("Undefined variable " ++ show other)
    EBinOp oper lhs rhs -> do
        lhs' <- evaluateExpr lhs
        rhs' <- evaluateExpr rhs
        case oper of
            Add -> return (lhs' + rhs')
            Sub -> return (lhs' - rhs')
            Mul -> return (lhs' * rhs')
            Div -> return (lhs' / rhs')
            Pow -> return (lhs' ** rhs')
            other -> throwError ("Invalid binary operator " ++ show other)
    EUnaOp oper expr -> do
        expr' <- evaluateExpr expr
        case oper of
            Sub -> return (negate expr')
            Add -> return expr'
            Fac -> return (fromInteger (product [1 .. (floor expr')]))
            other -> throwError ("Invalid unary operator " ++ show other)

evaluateLiteral :: Literal -> Double
evaluateLiteral = \case
    LInt n -> fromIntegral n
    LFloat n -> n

evaluate :: Expr -> Either Error Double
evaluate = first EvalError . runExcept . evaluateExpr
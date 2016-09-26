{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Evaluation where

import Calculator.Types
import Prelude hiding (Exp, exp)

evalT :: TExp -> Rational
evalT (TExp e) = eval e

evalU :: UExp -> Rational
evalU = evalT . toTExp

eval :: Exp a -> Rational
eval = \case
    Nat a -> fromIntegral a
    Neg a -> negate $ eval a
    Bra a -> eval a
    Add a b -> eval a + eval b
    Sub a b -> eval a - eval b
    Mul a b -> eval a * eval b
    Div a b -> eval a / eval b


{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Evaluation where

import Calculator.Value
import Calculator.Types
import Prelude hiding (Exp, exp)

evalT :: TExp -> Value
evalT (TExp e) = eval e

evalU :: UExp -> Value
evalU = evalT . toTExp

eval :: Exp a -> Value
eval = \case
    Val a -> a
    Neg a -> negate $ eval a
    Bra a -> eval a
    Add a b -> eval a + eval b
    Sub a b -> eval a - eval b
    Mul a b -> eval a * eval b
    Div a b -> eval a / eval b


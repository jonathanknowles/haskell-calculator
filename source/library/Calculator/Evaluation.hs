{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Evaluation where

import Calculator.Value
import Calculator.Types
import Prelude hiding (exp)

class Eval a where
    eval :: a -> Value

instance Eval TExp where
    eval (TExp e) = eval e

instance Eval UExp where
    eval = eval . toTExp

instance Eval (Exp a) where
    eval = \case
        Val a -> a
        Neg a -> negate $ eval a
        Bra a -> eval a
        Add a b -> eval a + eval b
        Sub a b -> eval a - eval b
        Mul a b -> eval a * eval b
        Div a b -> eval a / eval b


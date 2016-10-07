{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Printing where

import Calculator.Pretty
import Calculator.Tokens
import Calculator.Types

import Data.Monoid ((<>))

instance Pretty UExp where
    pretty = pretty . toTExp

instance Pretty TExp where
    pretty (TExp e) = pretty e

instance Pretty (Exp a) where
    pretty = \case
        Val a -> pretty a
        Neg a -> textNeg <> pretty a
        Bra a -> textBra <> pretty a <> textKet
        Add a b -> pretty a <> textAdd <> pretty b
        Sub a b -> pretty a <> textSub <> pretty b
        Mul a b -> pretty a <> textMul <> pretty b
        Div a b -> pretty a <> textDiv <> pretty b


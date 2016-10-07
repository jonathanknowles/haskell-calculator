{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Printing where

import Calculator.Evaluation
import Calculator.Tokens
import Calculator.Types
import Calculator.Value
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text as T

prettyU :: UExp -> Text
prettyU = prettyT . toTExp

prettyT :: TExp -> Text
prettyT (TExp e) = prettyE e

prettyE :: Exp a -> Text
prettyE = \case
    Val a -> prettyV a
    Neg a -> textNeg <> prettyE a
    Bra a -> textBra <> prettyE a <> textKet
    Add a b -> prettyE a <> textAdd <> prettyE b
    Sub a b -> prettyE a <> textSub <> prettyE b
    Mul a b -> prettyE a <> textMul <> prettyE b
    Div a b -> prettyE a <> textDiv <> prettyE b


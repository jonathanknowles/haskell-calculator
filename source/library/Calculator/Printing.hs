{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Calculator.Printing where

import Calculator.Tokens
import Calculator.Types
import Data.Monoid ((<>))
import Data.Ratio (denominator, numerator)
import Data.Text (Text)

import qualified Data.Text as T

prettyU :: UExp -> Text
prettyU = prettyT . toTExp

prettyT :: TExp -> Text
prettyT (TExp e) = pretty e

pretty :: Exp a -> Text
pretty = \case
    Nat a -> T.pack $ show a
    Neg a -> textNeg <> pretty a
    Bra a -> textBra <> pretty a <> textKet
    Add a b -> pretty a <> textAdd <> pretty b
    Sub a b -> pretty a <> textSub <> pretty b
    Mul a b -> pretty a <> textMul <> pretty b
    Div a b -> pretty a <> textDiv <> pretty b

prettyR :: Rational -> Text
prettyR r = if denominator r == 1 then n else n <> textDiv <> d
    where
        n = T.pack $ show $   numerator r
        d = T.pack $ show $ denominator r


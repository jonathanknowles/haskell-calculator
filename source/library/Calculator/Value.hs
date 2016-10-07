{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Calculator.Value
    ( Value
    , Error (..)
    , value
    ) where

import Calculator.Pretty
import Calculator.Tokens

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Data.Ratio (denominator, numerator)

import qualified Data.Text as T

newtype Value = V (Either Error Rational) deriving Eq

data Error = DivisionByZero deriving Eq

value :: (Error -> a) -> (Rational -> a) -> Value -> a
value f g (V e) = either f g e

instance Num Value where
    fromInteger a = V $ pure $ fromInteger a
    abs (V a)     = V $ abs    <$> a
    signum (V a)  = V $ signum <$> a
    negate (V a)  = V $ negate <$> a
    V a + V b     = V $ (+) <$> a <*> b
    V a * V b     = V $ (*) <$> a <*> b

instance Fractional Value where
    fromRational a = V $ pure $ fromRational a
    V (Left  e) / _           = V (Left e)
    _           / V (Left  e) = V (Left e)
    V (Right a) / V (Right b) = V $
        if b == 0
            then Left DivisionByZero
            else Right $ a / b

instance Show Value where
    show = T.unpack . pretty

instance Pretty Value where
    pretty = value
            (\DivisionByZero -> "<division by zero>")
            (\r -> if denominator r == 1
                       then n r
                       else n r <> textDiv <> d r)
        where
            n = T.pack . show . numerator
            d = T.pack . show . denominator


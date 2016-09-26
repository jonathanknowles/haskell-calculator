{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Calculator.Parsing where

import Calculator.Tokens
import Calculator.Types
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Attoparsec.Text
import Data.Text (Text)

import Prelude hiding (Exp, exp)

parseUExp :: Text -> Either String UExp
parseUExp = parseOnly uExp

uExp :: Parser UExp
uExp = choice [x <* endOfInput | x <- [bra, add, mul, neg, nat]]
    where
        nat = UNat <$> decimal
        neg = UNeg <$> choice [char charNeg *> x | x <- [bra, nat]]
        bra = choice [char charBra *> x <* char charKet | x <- [bra, add, mul, neg, nat]]
        add = chainL (choice [mul, nat, neg, bra]) addOp (choice [mul, nat, bra])
        mul = chainL (choice [nat, bra]) mulOp (choice [nat, bra])
        addOp = choice [char charAdd *> pure UAdd, char charSub *> pure USub]
        mulOp = choice [char charMul *> pure UMul, char charDiv *> pure UDiv]

chainL :: Parser b -> Parser (b -> a -> b) -> Parser a -> Parser b
chainL l o r = apply <$> l <*> o <*> r >>= rest
    where
        rest l = (apply l <$> o <*> r >>= rest) <|> return l
        apply l o r = l `o` r


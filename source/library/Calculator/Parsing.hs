{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Calculator.Parsing where

import Calculator.Tokens
import Calculator.Types
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Attoparsec.Text
import Data.Text (Text)

import Prelude hiding (exp)

import qualified Data.Text as T

errorBrackets = "expression contains unmatched brackets"
errorSyntax   = "expression contains syntax error"

parseUExp :: Text -> Either Text UExp
parseUExp e =
    if not (bracketsMatch e)
        then Left errorBrackets
        else either (const $ Left errorSyntax) Right (parseOnly uExp e)

bracketsMatch :: Text -> Bool
bracketsMatch t = T.foldl f 0 t == 0
    where
        f !a !c | a < 0     = a
                | c == '('  = a + 1
                | c == ')'  = a - 1
                | otherwise = a

uExp :: Parser UExp
uExp = choice [x <* endOfInput | x <- [bra, add, mul, neg, val]]
    where
        val = UVal . fromInteger <$> (ss *> decimal <* ss)
        neg = UNeg <$> (ss *> choice [char charNeg *> x | x <- [bra, val]])
        bra = ss *> choice [char charBra *> x <* char charKet | x <- [bra, add, mul, neg, val]] <* ss
        add = chainL (choice [mul, val, neg, bra]) addOp (choice [mul, val, bra])
        mul = chainL (choice [val, bra]) mulOp (choice [val, bra])
        addOp = choice [char charAdd *> pure UAdd, char charSub *> pure USub]
        mulOp = choice [char charMul *> pure UMul, char charDiv *> pure UDiv]
        ss = skipSpace

chainL :: Parser b -> Parser (b -> a -> b) -> Parser a -> Parser b
chainL l o r = apply <$> l <*> o <*> r >>= rest
    where
        rest l = (apply l <$> o <*> r >>= rest) <|> return l
        apply l o r = l `o` r


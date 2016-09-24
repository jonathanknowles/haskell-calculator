{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Calculator where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Ratio (numerator, denominator)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (Exp, exp)

-- Typed expression trees.

data Exp a where
    Nat :: !Natural -> Exp Nat
    Neg :: CNeg x => !(Exp x) -> Exp Neg
    Bra :: CBra x => !(Exp x) -> Exp Bra
    Add :: CAdd x y => !(Exp x) -> !(Exp y) -> Exp Add
    Sub :: CAdd x y => !(Exp x) -> !(Exp y) -> Exp Add
    Mul :: CMul x y => !(Exp x) -> !(Exp y) -> Exp Mul
    Div :: CMul x y => !(Exp x) -> !(Exp y) -> Exp Mul

class CNeg  a -- 'a' can be negated
class CBra  a -- 'a' can be nested within brackets
class CAddL a -- 'a' can be the left operand in an addition
class CAddR a -- 'a' can be the right operand in an addition
class CMulL a -- 'a' can be the left operand in a multiplication
class CMulR a -- 'a' can be the right operand in a multiplication

type CAdd a b = (CAddL a, CAddR b) -- 'a' and 'b' can be added together
type CMul a b = (CMulL a, CMulR b) -- 'a' can 'b' can be multiplied together

data Nat; instance CNeg Nat;                  ; instance CAddL Nat; instance CAddR Nat; instance CMulL Nat; instance CMulR Nat
data Neg;                  ; instance CBra Neg; instance CAddL Neg;                   ;                   ;
data Bra; instance CNeg Bra;                  ; instance CAddL Bra; instance CAddR Bra; instance CMulL Bra; instance CMulR Bra
data Add;                  ; instance CBra Add; instance CAddL Add;                   ;                   ;
data Mul;                  ; instance CBra Mul; instance CAddL Mul; instance CAddR Mul; instance CMulL Mul;

data TExp = forall a . TExp (Exp a)
data AddL = forall a . CAddL a => AddL (Exp a)
data AddR = forall a . CAddR a => AddR (Exp a)
data MulL = forall a . CMulL a => MulL (Exp a)
data MulR = forall a . CMulR a => MulR (Exp a)

addAny (AddL a) (AddR b) = Add a b
subAny (AddL a) (AddR b) = Sub a b
mulAny (MulL a) (MulR b) = Mul a b
divAny (MulL a) (MulR b) = Div a b

instance Eq (Exp a) where a == b = toUExp a == toUExp b
instance Eq TExp where (TExp a) == (TExp b) = toUExp a == toUExp b

deriving instance Show (Exp a)
instance Show TExp where show (TExp e) = show e

-- Untyped expression trees.

data UExp
    = UNat !Natural
    | UNeg !UExp
    | UAdd !UExp !UExp
    | USub !UExp !UExp
    | UMul !UExp !UExp
    | UDiv !UExp !UExp
    deriving (Eq, Show)

-- Converts expression trees from typed to untyped.

toUExp :: Exp a -> UExp
toUExp = \case
    Nat a -> UNat a
    Neg a -> UNeg (toUExp a)
    Bra a -> toUExp a
    Add a b -> UAdd (toUExp a) (toUExp b)
    Sub a b -> USub (toUExp a) (toUExp b)
    Mul a b -> UMul (toUExp a) (toUExp b)
    Div a b -> UDiv (toUExp a) (toUExp b)

-- Converts expression trees from untyped to typed.

toTExp :: UExp -> TExp
toTExp = \case
        UNat a   -> TExp $ Nat a
        UNeg a   -> TExp $ neg a
        UAdd a b -> TExp $ add a b
        USub a b -> TExp $ sub a b
        UMul a b -> TExp $ mul a b
        UDiv a b -> TExp $ div a b
    where
        neg = \case
            UNat a   -> Neg $ Nat a
            UNeg a   -> Neg $ Bra $ neg a
            UAdd a b -> Neg $ Bra $ add a b
            USub a b -> Neg $ Bra $ sub a b
            UMul a b -> Neg $ Bra $ mul a b
            UDiv a b -> Neg $ Bra $ div a b

        add a b = addAny (addL a) (addR b)
        sub a b = subAny (addL a) (addR b)
        mul a b = mulAny (mulL a) (mulR b)
        div a b = divAny (mulL a) (mulR b)

        addL = \case
            UNat a   -> AddL $ Nat a
            UNeg a   -> AddL $ neg a
            UAdd a b -> AddL $ add a b
            USub a b -> AddL $ sub a b
            UMul a b -> AddL $ mul a b
            UDiv a b -> AddL $ div a b

        addR = \case
            UNat a   -> AddR $ Nat a
            UNeg a   -> AddR $ Bra $ neg a
            UAdd a b -> AddR $ Bra $ add a b
            USub a b -> AddR $ Bra $ sub a b
            UMul a b -> AddR $ mul a b
            UDiv a b -> AddR $ div a b

        mulL = \case
            UNat a   -> MulL $ Nat a
            UNeg a   -> MulL $ Bra $ neg a
            UAdd a b -> MulL $ Bra $ add a b
            USub a b -> MulL $ Bra $ sub a b
            UMul a b -> MulL $ mul a b
            UDiv a b -> MulL $ div a b

        mulR = \case
            UNat a   -> MulR $ Nat a
            UNeg a   -> MulR $ Bra $ neg a
            UAdd a b -> MulR $ Bra $ add a b
            USub a b -> MulR $ Bra $ sub a b
            UMul a b -> MulR $ Bra $ mul a b
            UDiv a b -> MulR $ Bra $ div a b

-- Evaluation.

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

-- Pretty printing.

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

-- Fixed tokens.

charNeg = '-'; textNeg = T.singleton charNeg
charAdd = '+'; textAdd = T.singleton charAdd
charSub = '-'; textSub = T.singleton charSub
charMul = '*'; textMul = T.singleton charMul
charDiv = '/'; textDiv = T.singleton charDiv
charBra = '('; textBra = T.singleton charBra
charKet = ')'; textKet = T.singleton charKet
charEqu = '='; textEqu = T.singleton charEqu

-- Parsing.

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


{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Calculator.Types where

import Calculator.Value
import Prelude hiding (exp)

-- Typed expression trees.

data Exp a where
    Val :: !Value -> Exp Val
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

data Val; instance CNeg Val;                  ; instance CAddL Val; instance CAddR Val; instance CMulL Val; instance CMulR Val
data Neg;                  ; instance CBra Neg; instance CAddL Neg;                   ;                   ;
data Bra; instance CNeg Bra;                  ; instance CAddL Bra; instance CAddR Bra; instance CMulL Bra; instance CMulR Bra
data Add;                  ; instance CBra Add; instance CAddL Add;                   ;                   ;
data Mul;                  ; instance CBra Mul; instance CAddL Mul; instance CAddR Mul; instance CMulL Mul;

data TExp = forall a . TExp (Exp a)
data AddL = forall a . CAddL a => AddL !(Exp a)
data AddR = forall a . CAddR a => AddR !(Exp a)
data MulL = forall a . CMulL a => MulL !(Exp a)
data MulR = forall a . CMulR a => MulR !(Exp a)

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
    = UVal !Value
    | UNeg !UExp
    | UAdd !UExp !UExp
    | USub !UExp !UExp
    | UMul !UExp !UExp
    | UDiv !UExp !UExp
    deriving (Eq, Show)

{-| Converts expression trees from typed to untyped. -}
toUExp :: Exp a -> UExp
toUExp = \case
    Val a -> UVal a
    Neg a -> UNeg (toUExp a)
    Bra a -> toUExp a
    Add a b -> UAdd (toUExp a) (toUExp b)
    Sub a b -> USub (toUExp a) (toUExp b)
    Mul a b -> UMul (toUExp a) (toUExp b)
    Div a b -> UDiv (toUExp a) (toUExp b)

{-| Converts expression trees from untyped to typed. -}
toTExp :: UExp -> TExp
toTExp = \case
        UVal a   -> TExp $ Val a
        UNeg a   -> TExp $ neg a
        UAdd a b -> TExp $ add a b
        USub a b -> TExp $ sub a b
        UMul a b -> TExp $ mul a b
        UDiv a b -> TExp $ div a b
    where
        neg = \case
            UVal a   -> Neg $ Val a
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
            UVal a   -> AddL $ Val a
            UNeg a   -> AddL $ neg a
            UAdd a b -> AddL $ add a b
            USub a b -> AddL $ sub a b
            UMul a b -> AddL $ mul a b
            UDiv a b -> AddL $ div a b

        addR = \case
            UVal a   -> AddR $ Val a
            UNeg a   -> AddR $ Bra $ neg a
            UAdd a b -> AddR $ Bra $ add a b
            USub a b -> AddR $ Bra $ sub a b
            UMul a b -> AddR $ mul a b
            UDiv a b -> AddR $ div a b

        mulL = \case
            UVal a   -> MulL $ Val a
            UNeg a   -> MulL $ Bra $ neg a
            UAdd a b -> MulL $ Bra $ add a b
            USub a b -> MulL $ Bra $ sub a b
            UMul a b -> MulL $ mul a b
            UDiv a b -> MulL $ div a b

        mulR = \case
            UVal a   -> MulR $ Val a
            UNeg a   -> MulR $ Bra $ neg a
            UAdd a b -> MulR $ Bra $ add a b
            USub a b -> MulR $ Bra $ sub a b
            UMul a b -> MulR $ Bra $ mul a b
            UDiv a b -> MulR $ Bra $ div a b


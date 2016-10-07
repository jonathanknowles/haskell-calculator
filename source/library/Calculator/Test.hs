{-# LANGUAGE LambdaCase #-}

module Calculator.Test where

import Calculator.Parsing
import Calculator.Pretty
import Calculator.Printing
import Calculator.Tokens
import Calculator.Types
import Calculator.Value

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Test.QuickCheck

import qualified Data.Text as T

import Prelude hiding (exp)

main :: IO ()
main = do
    quickCheck propPrintParseIdentity
    quickCheck propNoDoubleOperators

{-| For any expression tree 't', pretty-printing the tree and then
    parsing the resultant text should result in an expression tree
    'u' that is structurally identical to 't'.
-}
propPrintParseIdentity :: UExp -> Bool
propPrintParseIdentity e =
    case parseUExp (pretty e) of
        Left e -> False
        Right r -> e == r

{-| Pretty-printing an expression tree should never produce a text
    where two or more operators appear in consecutive character
    positions.
-}
propNoDoubleOperators :: TExp -> Bool
propNoDoubleOperators e = not $ or
        [x `T.isInfixOf` pretty e | x <- doubleOperators]
    where
        doubleOperators = [ T.pack [ x, y ]
                          | x <- singleOperators
                          , y <- singleOperators ]
        singleOperators = [ charNeg
                          , charAdd
                          , charSub
                          , charMul
                          , charDiv ]

instance Arbitrary UExp where
    arbitrary = sized tree
        where
            tree 0 = UVal <$> arbitrary
            tree n = oneof
                    [ UNeg <$> x
                    , UAdd <$> x <*> x
                    , USub <$> x <*> x
                    , UMul <$> x <*> x
                    , UDiv <$> x <*> x ]
                where
                    x = tree $ n * 2 `div` 3
    shrink = \case
            UVal n -> UVal <$> shrink n
            UNeg a -> a : (UNeg <$> shrink a)
            UAdd a b -> s UAdd a b
            USub a b -> s USub a b
            UMul a b -> s UMul a b
            UDiv a b -> s UDiv a b
        where
            s op a b =
                (a : ((a `op`) <$> shrink b)) <>
                (b : ((`op` b) <$> shrink a))

instance Arbitrary TExp where
    arbitrary = toTExp <$> arbitrary

instance Arbitrary Value where
    arbitrary = fromInteger <$> arbitrary


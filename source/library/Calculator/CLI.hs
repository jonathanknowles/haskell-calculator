{-# LANGUAGE OverloadedStrings #-}

module Calculator.CLI where

import Calculator.Evaluation
import Calculator.Parsing
import Calculator.Pretty
import Calculator.Printing
import Calculator.Tokens

import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    T.putStrLn "Please enter arithmetic expressions to have them evaluated."
    forever $ do
        T.putStr "> "
        T.putStrLn . calculate =<< T.getLine

{-| Parses the given expression, evaluates the resulting
    expression tree, and then pretty prints the result. -}
calculate :: Text -> Text
calculate e =
    case parseExpression e of
        ExpressionParseFailure e -> pretty e
        ExpressionParseSuccess r -> pretty r <> " " <> textEqu <> " " <> pretty (eval r)


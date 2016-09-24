{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calculator
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
        T.getLine >>= T.putStrLn . calculate . stripSpaces

stripSpaces :: Text -> Text
stripSpaces = T.filter (/= ' ')

{-| Parses the given expression, evaluates the resulting
    expression tree, and then pretty prints the result. -}
calculate :: Text -> Text
calculate e =
    case parseUExp e of
        Left e -> "Syntax error! Please try again."
        Right r -> prettyU r <> textEqu <> prettyR (evalU r)


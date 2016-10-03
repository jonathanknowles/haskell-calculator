{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}

module Calculator.WebInterface where

import Calculator.Evaluation
import Calculator.Parsing
import Calculator.Printing
import Calculator.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, forM_)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Data.Text (Text)

import Reflex
import Reflex.Dom

import qualified Data.Text as T

css = encodeUtf8 $ T.unlines
    [ "* {font: 16pt sans-serif}"
    , "body {background-color: #aaaaaa}"
    , "table {border: solid 0.2em; text-align: center; padding: 0.2em; float: center}" 
    , "td {text-align: center; vertical-align: center}"         
    , "tr {padding: 0em; border: 0em; margin: 0em}"
    , "table.add {border-color: #ff0000; background-color: #ff8888}"
    , "table.sub {border-color: #008800; background-color: #88ff88}"
    , "table.mul {border-color: #0000ff; background-color: #8888ff}"
    , "table.div {border-color: #bb00bb; background-color: #bb88bb}"
    , "table.nat {border-color: #444444; background-color: #ffffff}"
    , "table.neg {border-color: #008800; background-color: #88ff88}"
    , "input {border: solid 0.2em; padding: 0.2em; width: 90%}"
    , "input.valid {border-color: #444444}"
    , "input.error {border-color: #ff8888}"
    , "div.result {border: solid 0.2em #444444; background-color: #ffffff; padding: 0.2em; width: 90%}"
    ]

main = mainWidgetWithCss css $ el "div" $ do
    z <- expInput
    verticalSpace
    dyn =<< mapDyn evaluateParseResult z
    verticalSpace
    dyn =<< mapDyn renderParseResult z
    verticalSpace

verticalSpace =  el "div" $ el "p" $ text ""

type ParseResult = Either String UExp

expInput :: MonadWidget t m => m (Dynamic t ParseResult)
expInput = do  
        rec attrs  <- mapDyn (either (const $ "class" =: "error")
                                     (const $ "class" =: "valid")) result
            result <- mapDyn parseUExp $ _textInput_value n
            n <- textInput $ def & textInputConfig_initialValue .~ "0"
                                 & textInputConfig_attributes   .~ attrs
        return result

evaluateParseResult :: MonadWidget t m => ParseResult -> m ()
evaluateParseResult = elAttr "div" ("class" =: "result") . text .
    either
        (const "Please enter a valid expression.")
        (prettyR . evalU)

symbolAdd = "+"
symbolSub = "−"
symbolMul = "×"
symbolDiv = "÷"
symbolBra = "("
symbolKet = ")"
symbolNeg = symbolSub

renderParseResult :: MonadWidget t m => ParseResult -> m ()
renderParseResult = either (const $ el "div" $ text "") render
    where
        render :: MonadWidget t m => UExp -> m ()
        render = \case
                UNat a -> elAttr "table" ("class" =: "nat") $ el "tr" $ text $ T.pack $ show a
                UNeg a -> elAttr "table" ("class" =: "neg") $ el "tr" $ do el "td" $ text symbolNeg
                                                                           el "td" $ render a
                UAdd a b -> binop "add" symbolAdd a b
                USub a b -> binop "sub" symbolSub a b 
                UMul a b -> binop "mul" symbolMul a b 
                UDiv a b -> binop "div" symbolDiv a b
            where
                binop c o a b =
                    elAttr "table" ("class" =: c) $
                        el "tr" $ do
                            el "td" $ render a
                            el "td" $ text o
                            el "td" $ render b


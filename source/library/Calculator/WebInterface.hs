{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}

module Calculator.WebInterface where

import Calculator.Evaluation
import Calculator.Parsing
import Calculator.Pretty
import Calculator.Printing
import Calculator.Types
import Calculator.Value
import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Reflex
import Reflex.Class (fmapMaybe)
import Reflex.Dom

import qualified Data.Text as T

css :: Text
css = T.unlines
    [ "* {font-size: 12pt; font-family: 'Droid Sans', sans-serif}"
    , "body {background-color: #c0c0d0; padding: 1em; margin: 0em}"
      -- Links
    , "a {color: #4040a0}"
    , "a:link {text-decoration: none}"
    , "a:visited {text-decoration: none}"
    , "a:hover {text-decoration: underline}"
    , "a:active {text-decoration: underline}"
      -- Paragraphs
    , "p {margin: 0em 0em 1em 0em}"
    , "ul {margin: 0.2em 0em 0.2em 1em}"
      -- Tables
    , "table {border: solid 0.2em; text-align: center; padding: 0.2em; border-radius: 0.4em}"
    , "table.add {border-color: #ff0000; background-color: #ff8888}"
    , "table.sub {border-color: #008800; background-color: #88ff88}"
    , "table.mul {border-color: #0000ff; background-color: #8888ff}"
    , "table.div {border-color: #bb00bb; background-color: #bb88bb}"
    , "table.neg {border-color: #008800; background-color: #88ff88}"
    , "table.val {border-color: #444444; background-color: #ffffff}"
    , "table.val {padding: 0.2em 0.4em 0.2em 0.4em}"
    , "td {text-align: center; vertical-align: center}"
    , "tr {padding: 0em; border: 0em; margin: 0em}"
      -- Inputs
    , "input {border: solid 0.2em; border-radius: 0.4em; padding: 0.2em 0.4em 0.2em 0.4em; width: 90%}"
    , "input:focus {outline: none}"
    , "input.valid {border-color: #444444}"
    , "input.empty {border-color: #444444}"
    , "input.error {border-color: #ff0000}"
      -- Sections
    , "div {padding: 0em; margin: 0em}"
    , "div.result {padding-top: 0.4em}"
    , "div.feedback {padding-top: 0.2em; font-style: italic; font-weight: bold}"
    , "div.heading {padding-top: 0.4em; padding-bottom: 0.2em; font-weight: bold}"
    , "div.value {border: solid 0.2em #444444; border-radius: 0.4em}"
    , "div.value {background-color: #ffffff; padding: 0.2em 0.4em 0.2em 0.4em; width: 90%}"
    ]

main :: IO ()
main = mainWidgetWithHead headSection $ do
        introduction
        parseResult <- expressionInput
        maybeExpression <- holdDyn Nothing $ removeInvalidExpressions parseResult
        dyn $ maybeEvaluateExpression <$> maybeExpression
        pure ()
    where
        maybeEvaluateExpression = maybe
            help
            evaluateExpression
        removeInvalidExpressions = fmapMaybe
            (maybeEither
                (Just Nothing)
                (const Nothing)
                (Just . Just)) . updated

headSection :: MonadWidget t m => m ()
headSection = do
    elAttr "link" ( "rel"  =: "stylesheet" <>
                    "type" =: "text/css"   <>
                    "href" =: "https://fonts.googleapis.com/css?family=Droid+Sans" ) $ text ""

    elAttr "style" ( "type" =: "text/css" ) $ text css

introduction :: MonadWidget t m => m ()
introduction = el "div" $
        el "p" $ do
            text "A "
            el "strong" $ text "calculator"
            text " implemented in "
            elAttr "a" ("href" =: "https://www.haskell.org/") $ text "Haskell"
            text ", "
            elAttr "a" ("href" =: "https://github.com/ghcjs/ghcjs") $ text "GHCJS"
            text ", and "
            elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-platform") $ text "Reflex"
            text ". ("
            el "strong" $
                elAttr "a" ("href" =: "https://github.com/jonathanknowles/haskell-calculator") $ text "View source code"
            text ")"

help :: MonadWidget t m => m ()
help = elAttr "div" ("class" =: "help") $
    el "p" $ do
        text "This calculator supports:"
        el "ul" $ do
            el "li" $ text "addition, subtraction, multiplication and division"
            el "li" $ text "natural numbers"
            el "li" $ text "parentheses"

expressionInput :: MonadWidget t m => m (Dynamic t (Maybe (Either Text UExp)))
expressionInput = do
        elAttr "div" ("class" =: "heading") $ text "Enter an arithmetic expression:"
        rec t <- el "div" $ textInput $ def & textInputConfig_initialValue .~ T.empty
                                            & textInputConfig_attributes   .~ c
            let c = resultClass <$> r
                r = maybeParse <$> _textInput_value t
            elAttr "div" ("class" =: "feedback") $ dyn $ feedback <$> r
        return r
    where
        maybeParse x = if T.null x then Nothing
                                   else Just $ parseUExp x
        feedback = text .
            maybeEither hardSpace id (const hardSpace)
        resultClass = ("class" =:) . maybeEither "empty"
                                          (const "error")
                                          (const "valid")

maybeEither :: c -> (a -> c) -> (b -> c) -> Maybe (Either a b) -> c
maybeEither n l r = \case
    Nothing        -> n
    Just (Left  x) -> l x
    Just (Right x) -> r x

maybeEither' :: Maybe (Either a b) -> Maybe b
maybeEither' = maybeEither Nothing (const Nothing) Just

evaluateExpression :: MonadWidget t m => UExp -> m ()
evaluateExpression e =
    elAttr "div" ("class" =: "result") $ do
        elAttr "div" ("class" =: "heading") $ text "Result:"
        elAttr "div" ("class" =: "value"  ) $ text $ pretty $ eval $ e
        elAttr "div" ("class" =: "heading") $ text "Visualization:"
        elAttr "div" ("class" =: "graphic") $ renderExpression e

renderExpression :: MonadWidget t m => UExp -> m ()
renderExpression = \case
        UVal a -> elAttr "table" ("class" =: "val") $
                      el "tr" $ text $ pretty a
        UNeg a -> elAttr "table" ("class" =: "neg") $
                      el "tr" $ do el "td" $ text symbolNeg
                                   el "td" $ renderExpression a
        UAdd a b -> binop "add" symbolAdd a b
        USub a b -> binop "sub" symbolSub a b
        UMul a b -> binop "mul" symbolMul a b
        UDiv a b -> binop "div" symbolDiv a b
    where
        binop c o a b =
            elAttr "table" ("class" =: c) $
                el "tr" $ do
                    el "td" $ renderExpression a
                    el "td" $ text o
                    el "td" $ renderExpression b

hardSpace = "　"
symbolAdd = "+"
symbolSub = "−"
symbolMul = "×"
symbolDiv = "÷"
symbolBra = "("
symbolKet = ")"
symbolNeg = symbolSub


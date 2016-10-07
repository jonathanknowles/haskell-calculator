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
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Reflex
import Reflex.Class (fmapMaybe)
import Reflex.Dom

import qualified Data.Text as T

css = encodeUtf8 $ T.unlines
    [ "* {font: 15pt sans-serif}"
    , "body {background-color: #aaaaaa; padding: 1em; margin: 0em}"
    , "table {border: solid 0.2em; text-align: center; padding: 0.2em}" 
    , "td {text-align: center; vertical-align: center}"         
    , "tr {padding: 0em; border: 0em; margin: 0em}"
    , "table.add {border-color: #ff0000; background-color: #ff8888}"
    , "table.sub {border-color: #008800; background-color: #88ff88}"
    , "table.mul {border-color: #0000ff; background-color: #8888ff}"
    , "table.div {border-color: #bb00bb; background-color: #bb88bb}"
    , "table.val {border-color: #444444; background-color: #ffffff}"
    , "table.neg {border-color: #008800; background-color: #88ff88}"
    , "input {border: solid 0.2em; padding: 0.2em; width: 90%}"
    , "input.valid {border-color: #444444}"
    , "input.empty {border-color: #444444}"
    , "input.error {border-color: #ff0000}"
    , "div.result {padding-top: 1em}"
    , "div.feedback {font-weight: bolder; padding-top: 0.2em}"
    , "div.heading {padding-top: 0.8em; padding-bottom: 0.2em}"
    , "div.value {border: solid 0.2em #444444; background-color: #ffffff; padding: 0.2em; width: 90%}"
    ]

main = mainWidgetWithCss css $ el "div" $ do
        parseResult <- expressionInput
        maybeExpression <- holdDyn Nothing $ removeInvalidExpressions parseResult
        dyn $ maybeEvaluateExpression <$> maybeExpression
        pure ()
    where
        maybeEvaluateExpression = maybe
            (pure ())
            evaluateExpression
        removeInvalidExpressions = fmapMaybe
            (maybeEither
                (Just Nothing)
                (const Nothing)
                (Just . Just)) . updated

expressionInput :: MonadWidget t m => m (Dynamic t (Maybe (Either Text UExp)))
expressionInput = do
        elAttr "div" ("class" =: "heading") $ text "Enter an arithmetic expression:"
        rec t <- el "div" $ textInput $ def & textInputConfig_initialValue .~ T.empty
                                            & textInputConfig_attributes   .~ c
            let c = resultClass <$> r
                r = maybeParse <$> _textInput_value t
            elAttr "div" ("class" =: "feedback") $ dyn $ feedbackText <$> r
        return r
    where
        maybeParse x = if T.null x then Nothing
                                   else Just $ parseUExp x
        feedbackText =
            text . maybeEither hardSpace id (const hardSpace)
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
        elAttr "div" ("class" =: "heading") $ text "Expression:"
        elAttr "div" ("class" =: "value"  ) $ text $ pretty $ e 
        elAttr "div" ("class" =: "heading") $ text "Value:"
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


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
import Prelude hiding (head)
import Reflex
import Reflex.Class (fmapMaybe)
import Reflex.Dom
import Reflex.Dom.Extras

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Page structure
-------------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithHead head body

head :: MonadWidget t m => m ()
head = do styleSheet "https://fonts.googleapis.com/css?family=Droid+Sans"
          styleSheet "style.css"

body :: MonadWidget t m => m ()
body = do introduction
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

introduction :: DomBuilder t m => m ()
introduction = el "div" $ el "p" $ do
        text "A "
        el "strong" $ text "calculator"
        text " implemented in "
        link urlHaskell $ text "Haskell"
        text ", "
        link urlGhcjs $ text "GHCJS"
        text ", and "
        link urlReflex $ text "Reflex"
        text ". ("
        el "strong" $ link urlSource $ text "View source code"
        text ")"
    where
        link href  = elAttr "a" ("href" =: href)
        urlHaskell = "https://www.haskell.org/"
        urlGhcjs   = "https://github.com/ghcjs/ghcjs"
        urlReflex  = "https://github.com/reflex-frp/reflex-platform"
        urlSource  = "https://github.com/jonathanknowles/haskell-calculator"

help :: MonadWidget t m => m ()
help = divClass "help" $
    el "p" $ do
        text "This calculator supports:"
        el "ul" $ do
            el "li" $ text "addition, subtraction, multiplication and division"
            el "li" $ text "natural numbers"
            el "li" $ text "parentheses"

expressionInput :: MonadWidget t m => m (Dynamic t (Maybe (Either Text UExp)))
expressionInput = do
        divClass "heading" $ text "Enter an arithmetic expression:"
        rec t <- el "div" $ textInput $ def & textInputConfig_initialValue .~ T.empty
                                            & textInputConfig_attributes   .~ c
            let c = resultClass <$> r
                r = maybeParse <$> _textInput_value t
            divClass "feedback" $ dyn $ feedback <$> r
        return r
    where
        maybeParse x = if T.null x then Nothing
                                   else Just $ parseUExp x
        feedback = text .
            maybeEither hardSpace id (const hardSpace)
        resultClass = ("class" =:) . maybeEither "empty"
                                          (const "error")
                                          (const "valid")

evaluateExpression :: MonadWidget t m => UExp -> m ()
evaluateExpression e =
    divClass "result" $ do
        divClass "heading" $ text "Result:"
        divClass "value"   $ text $ pretty $ eval $ e
        divClass "heading" $ text "Visualization:"
        divClass "graphic" $ renderExpression e

renderExpression :: MonadWidget t m => UExp -> m ()
renderExpression = \case
        UVal a -> tableClass "val" $ tr $ text $ pretty a
        UNeg a -> tableClass "neg" $ tr $ do td $ text symbolNeg
                                             td $ renderExpression a
        UAdd a b -> binop "add" symbolAdd a b
        USub a b -> binop "sub" symbolSub a b
        UMul a b -> binop "mul" symbolMul a b
        UDiv a b -> binop "div" symbolDiv a b
    where
        binop c o a b =
            tableClass c $ tr $ do
                td $ renderExpression a
                td $ text o
                td $ renderExpression b

-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

hardSpace = "　"
symbolAdd = "+"
symbolSub = "−"
symbolMul = "×"
symbolDiv = "÷"
symbolBra = "("
symbolKet = ")"
symbolNeg = symbolSub

-------------------------------------------------------------------------------
-- DOM builders
-------------------------------------------------------------------------------

tableClass c = elAttr "table" ("class" =: c)

tr = el "tr"
td = el "td"

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

maybeEither :: c -> (a -> c) -> (b -> c) -> Maybe (Either a b) -> c
maybeEither n l r = \case
    Nothing        -> n
    Just (Left  x) -> l x
    Just (Right x) -> r x

maybeEither' :: Maybe (Either a b) -> Maybe b
maybeEither' = maybeEither Nothing (const Nothing) Just


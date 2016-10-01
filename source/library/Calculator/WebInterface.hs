{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE OverloadedStrings         #-}

module Calculator.WebInterface where

import Calculator.Evaluation
import Calculator.Parsing
import Calculator.Printing
import Calculator.Types

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex
import Reflex.Dom

import qualified Data.Map  as M
import qualified Data.Text as T

main = mainWidget $ el "div" $ do
    z <- expInput
    x <- mapDyn (either (const "Syntax error") (\u -> prettyU u <> " = " <> prettyR (evalU u))) z
    dynText x

expInput :: (MonadWidget t m) => m (Dynamic t (Either String UExp))
expInput = do
        rec n <- textInput $ def & textInputConfig_initialValue .~ "0"
                                 & textInputConfig_attributes   .~ attrs
            
            attrs  <- mapDyn (either (const styleError) (const styleValid)) result
            result <- mapDyn parseUExp $ _textInput_value n
        return result

styleError = M.singleton "style" "border-width: 1em; width: 20em; border-style: solid; border-color: red;  "  
styleValid = M.singleton "style" "border-width: 1em; width: 20em; border-style: solid; border-color: green;"
  

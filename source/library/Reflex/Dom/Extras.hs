{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Extras where

import Data.Text (Text)
import Reflex.Dom

styleSheet :: DomBuilder t m => Text -> m ()
styleSheet link = elAttr "link" as $ pure ()
    where as = mconcat [ "rel"  =: "stylesheet"
                       , "type" =: "text/css"
                       , "href" =: link ]


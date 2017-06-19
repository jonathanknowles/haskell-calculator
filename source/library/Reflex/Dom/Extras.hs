{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Reflex.Dom.Extras where

import Data.Text (Text)
import Reflex.Dom

styleSheet :: DomBuilder t m => Text -> m ()
styleSheet link = elAttr "link" as $ pure ()
    where as = mconcat [ "rel"  =: "stylesheet"
                       , "type" =: "text/css"
                       , "href" =: link ]

tableClass c = elAttr "table" ("class" =: c)

a href = elAttr "a" ("href" =: href)

div    = el "div"
tr     = el "tr"
td     = el "td"
ul     = el "ul"
li     = el "li"
p      = el "p"
strong = el "strong"


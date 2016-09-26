{-# LANGUAGE OverloadedStrings #-}

module Calculator.Tokens where

import qualified Data.Text as T

charNeg = '-'; textNeg = T.singleton charNeg
charAdd = '+'; textAdd = T.singleton charAdd
charSub = '-'; textSub = T.singleton charSub
charMul = '*'; textMul = T.singleton charMul
charDiv = '/'; textDiv = T.singleton charDiv
charBra = '('; textBra = T.singleton charBra
charKet = ')'; textKet = T.singleton charKet
charEqu = '='; textEqu = T.singleton charEqu


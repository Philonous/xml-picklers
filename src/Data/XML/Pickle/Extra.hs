{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.XML.Pickle.Extra where

import Data.XML.Pickle.XpNTuple
import Data.XML.Pickle
import Data.Text (pack)

$(xpTuples 10) -- generates xpNTuple where 6 < n < 62 (max of GHC)


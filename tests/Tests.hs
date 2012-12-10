{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}

module Tests where

import Data.Monoid
import Data.XML.Types

import Data.XML.Pickle
import qualified Data.Text as Text

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main = flip defaultMainWithOpts (mempty {ropt_color_mode = Just ColorNever}) $
  [ testGroup "xpUnit" [ testProperty "xpUnit1" testxpUnit1
                       , testProperty "xpUnit2" testxpUnit2
                       ]
  , testGroup "xpId"        [ testProperty "xpId" testxpId]
  , testGroup "combinators" [ testProperty "xpIsolate" testxpIsolate
                            , testProperty "xpIsolatePair" testxpIsolatePair
                            , testProperty "pair leniency" testPairLenient
                            ]


  ]

testxpUnit1 xs  = pickle xpUnit () == ([] :: [Int])
    where types = xs :: Int

testxpUnit2 xs  = unpickle xpUnit xs == Right ()
    where types = xs :: [Int]

testxpId xs = null xs .||. (unpickle xpId xs == Right xs)
    where types = xs :: [Int]

testxpIsolate xs = null xs .||.
                     (unpickle (xpIsolate xpId) xs == Right (take 1 xs))
      where types = xs :: [Int]

testxpIsolatePair xs = null xs .||.
                     (unpickle (xpPair (xpIsolate xpId)
                                       (xpId)
                               ) xs == Right (take 1 xs, drop 1 xs))
      where types = xs :: [Int]

testPairLenient xs = unpickle (xpId <++> xpUnit) xs == Right (xs, ())
      where types = xs :: [Int]

el x = Element "foo" [("bar",(return . ContentText  $ "blub" `Text.append` x))] []
el' xs = Element "parent" [] $ flip map xs (\x -> NodeElement $ el (Text.pack $ show x))

doc = el' [1..5]


testDoc = xpRoot $ xpElem "parent" xpUnit (xpAll $ xpElem "foo" xpId xpUnit)

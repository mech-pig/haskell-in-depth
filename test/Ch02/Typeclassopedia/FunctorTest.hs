{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.FunctorTest where

import Ch02.Typeclassopedia.Data
  ( Either (..),
    FunctionApplication (..),
    Maybe (..),
    Pair (..),
    Tuple (..),
    ZipList (..),
    apply,
  )
import Ch02.Typeclassopedia.Functor ()
import Data.Bool (Bool (..))
import Data.Foldable (length)
import Data.Functor (Functor, (<$>))
import Data.Int (Int)
import Data.String (String)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (($), (+), (.), (==))

suite :: TestTree
suite =
  testGroup
    "Functor"
    [ maybeSuite,
      eitherSuite,
      functionApplicationSuite,
      tupleSuite,
      pairSuite,
      zipListSuite
    ]

maybeSuite :: TestTree
maybeSuite =
  testGroup
    "Maybe"
    [ testGroup
        "fmap"
        [ testCase "nothing is returned" $
            length <$> (Nothing :: Maybe String) @?= Nothing,
          testCase "just applies the function to its value" $
            length <$> Just "test" @?= Just 4
        ]
    ]

eitherSuite :: TestTree
eitherSuite = testGroup "Either e" [fmapTests]
  where
    fmapTests =
      testGroup
        "fmap"
        [ testCase "left e is returned" $
            (+) 1 <$> Left "ko" @?= (Left "ko" :: Either String Int),
          testCase "right applies the function to its value" $
            (+) 1 <$> Right 5 @?= (Right 6 :: Either String Int)
        ]

functionApplicationSuite :: TestTree
functionApplicationSuite =
  testGroup
    "(->) r"
    [ testGroup
        "fmap"
        [ testCase "is equal to function composition" $ do
            apply ((+) 3 <$> FunctionApplication length) "test" @?= apply (FunctionApplication ((+) 3 . length)) "test"
            apply ((+) 3 <$> FunctionApplication length) "test" @?= 7
        ]
    ]

tupleSuite :: TestTree
tupleSuite =
  testGroup
    "(,) e"
    [ testGroup
        "fmap"
        [ testCase "(e, func a) is returned" $
            (+) 5 <$> Tuple ("fixed", 4) @?= Tuple ("fixed", 9)
        ]
    ]

pairSuite :: TestTree
pairSuite =
  testGroup
    "Pair"
    [ testGroup
        "fmap"
        [ testCase "Pair is returned with function applied to both members" $
            (==) 5 <$> Pair (4, 5) @?= Pair (False, True)
        ]
    ]

zipListSuite :: TestTree
zipListSuite =
  testGroup
    "ZipList"
    [ testGroup
        "fmap"
        [ testCase "empty list" $
            length <$> ZipList ([] :: [String]) @?= ZipList [],
          testCase "singleton list" $
            length <$> ZipList ["test"] @?= ZipList [4],
          testCase "len > 1 list" $
            length <$> ZipList ["test", "test2", "test"] @?= ZipList [4, 5, 4]
        ]
    ]
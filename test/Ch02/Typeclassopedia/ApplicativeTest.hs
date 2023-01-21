{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.ApplicativeTest where

import Ch02.Typeclassopedia.Applicative (sequenceAL)
import Ch02.Typeclassopedia.Data (Maybe (..), ZipList (..))
import Ch02.Typeclassopedia.TestUtils ((@?=*))
import Control.Applicative (Applicative (pure, (<*>)))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (map, take)
import Data.String (String)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Prelude (Eq, Foldable (length), Show, id, show, ($), (*), (+), (++), (-), (.))

suite :: TestTree
suite =
  testGroup
    "Applicative"
    [ maybeSuite,
      zipListSuite,
      sequenceALSuite
    ]

-- applying non-effectful identity to an effectful argument
-- in an effectful context is equivalent to the effectful argument
assertLawIdentity :: (Eq (f a), Show (f a), Applicative f) => f a -> Assertion
assertLawIdentity fa = pure id <*> fa @?= fa

-- applying a non-effectful function to a non-effectful argument
-- in an effectful context is the same as just applying the function
-- to the argument and then injecting the result into the context with pure
assertLawHomomorphism :: forall f a b. (Eq (f a), Show (f a), Applicative f) => (b -> a) -> b -> Assertion
assertLawHomomorphism f g = pure @f f <*> pure g @?=* pure (f g)

-- when evaluating the application of an effectful function to a pure argument,
-- the order in which we evaluate the function and its argument doesn't matter
assertLawInterchange :: (Eq (f a), Show (f a), Applicative f) => f (b -> a) -> b -> Assertion
assertLawInterchange u y = u <*> pure y @?= pure (\f -> f y) <*> u

-- this law expresses a sort of associativity property of (<*>)
assertLawComposition :: (Eq (f a), Show (f a), Applicative f) => f (b -> a) -> f (c -> b) -> f c -> Assertion
assertLawComposition u v w = u <*> (v <*> w) @?= pure (.) <*> u <*> v <*> w

maybeSuite :: TestTree
maybeSuite =
  testGroup
    "Maybe"
    [ testGroup
        "pure"
        [ testCase "Maybe Int" $ pure 5 @?= Just 5,
          testCase "Maybe String" $ pure "test" @?= Just "test"
        ],
      testGroup
        "<*>"
        [ testCase "function is Nothing" $ (Nothing :: Maybe (Int -> String)) <*> Just 5 @?= Nothing,
          testCase "arg is Nothing" $ Just (1 +) <*> Nothing @?= Nothing,
          testCase "arg and function are Nothing" $ (Nothing :: Maybe (Int -> String)) <*> Nothing @?= Nothing
        ],
      testGroup
        "identity law"
        [ testCase "Just" $ assertLawIdentity $ Just "test",
          testCase "Nothing" $ assertLawIdentity (Nothing :: Maybe String)
        ],
      testGroup
        "homomorphism law"
        [ testCase "String -> Int" $ assertLawHomomorphism @Maybe length "test",
          testCase "Int -> Int" $ assertLawHomomorphism @Maybe (+ 2) 4
        ],
      testGroup
        "interchange law"
        [ testCase "Just" $ assertLawInterchange (Just length) "test",
          testCase "Nothing" $ assertLawInterchange (Nothing :: Maybe (String -> Int)) "test"
        ],
      testGroup
        "composition law"
        [ testCase "Just" $ assertLawComposition (Just length) (Just show) (Just 42),
          testCase "Nothing (1st function)" $ assertLawComposition (Nothing :: Maybe (String -> Int)) (Just show) (Just 42),
          testCase "Nothing (2nd function)" $ assertLawComposition (Just length) (Nothing :: Maybe (Int -> String)) (Just 42),
          testCase "Nothing (argument)" $ assertLawComposition (Just length) (Just show) (Nothing :: Maybe Int)
        ]
    ]

zipListSuite :: TestTree
zipListSuite =
  testGroup
    "ZipList"
    [ testCase
        "pure"
        $ (take 1 . unZipList) (pure @ZipList "test") @?= ["test"], -- workaround to avoid infinite recursion caused by printing "repeat"
      testGroup
        "<*>"
        $ map
          (\(testName, fba, fb, fa) -> testCase testName $ fba <*> fb @?= fa)
          [ ("size functions = size inputs = 0", ZipList [], ZipList [], ZipList []),
            ("size functions > 1, size inputs = 0", ZipList [(+ 1), (-) 1, (* 3)], ZipList [], ZipList []),
            ("size functions = 0, size inputs > 1", ZipList [], ZipList [1, 2, 3], ZipList []),
            ("size functions = size inputs = 1", ZipList [(* 3)], ZipList [2], ZipList [6]),
            ("size functions > 1, size inputs = 1", ZipList [(+ 1), (-) 1, (* 1)], ZipList [2], ZipList [3]),
            ("size functions = 1, size inputs > 1", ZipList [(+ 1)], ZipList [1, 2, 3], ZipList [2]),
            ("size functions = size inputs > 1", ZipList [(+ 1), (* 3)], ZipList [2, 3], ZipList [3, 9]),
            ("size functions > size inputs > 1", ZipList [(+ 1), (-) 2, (* 3)], ZipList [2, 3], ZipList [3, -1]),
            ("size inputs > size functions > 1", ZipList [(-) 2, (* 3)], ZipList [1, 2, 3], ZipList [1, 6])
          ],
      testGroup
        "identity law"
        [ testCase "size = 0" $ assertLawIdentity (ZipList [] :: ZipList Int),
          testCase "size = 1" $ assertLawIdentity (ZipList [1] :: ZipList Int),
          testCase "size > 1" $ assertLawIdentity (ZipList [1, 2] :: ZipList Int)
        ],
      testGroup
        "homomorphism law"
        [],
      -- the execution of assertLawHomomorphism causes infinite recursion
      -- testCase "String -> Int" $ assertLawHomomorphism @ZipList length "test",
      -- testCase "Int -> Int" $ assertLawHomomorphism @ZipList (+ 2) 4

      testGroup
        "interchange law"
        [ testCase "size = 0" $ assertLawInterchange (ZipList [] :: ZipList (String -> Int)) "test",
          testCase "size = 1" $ assertLawInterchange (ZipList [length]) "test",
          testCase "size > 1" $ assertLawInterchange (ZipList [length, (+ 2) . length]) "test"
        ],
      testGroup
        "composition law"
        [ -- singletons
          testCase "size = 0, size = 0, size = 0" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 0, size = 0, size = 1" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [1] :: ZipList Int),
          testCase "size = 0, size = 1, size = 0" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [show] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 0, size = 1, size = 1" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [show] :: ZipList (Int -> String)) (ZipList [1] :: ZipList Int),
          testCase "size = 1, size = 0, size = 0" $ assertLawComposition (ZipList [length] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 1, size = 0, size = 1" $ assertLawComposition (ZipList [length] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [1] :: ZipList Int),
          testCase "size = 1, size = 1, size = 0" $ assertLawComposition (ZipList [length] :: ZipList (String -> Int)) (ZipList [show] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 1, size = 1, size = 1" $ assertLawComposition (ZipList [length] :: ZipList (String -> Int)) (ZipList [show] :: ZipList (Int -> String)) (ZipList [1] :: ZipList Int),
          -- size > 1
          testCase "size = 0, size = 0, size > 1" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [1, 2] :: ZipList Int),
          testCase "size = 0, size > 1, size = 0" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [show, (++ "test") . show] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 0, size > 1, size > 1" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [show, (++ "test") . show] :: ZipList (Int -> String)) (ZipList [1, 2] :: ZipList Int),
          testCase "size > 1, size = 0, size = 0" $ assertLawComposition (ZipList [length, (+ 1) . length] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size > 1, size = 0, size > 1" $ assertLawComposition (ZipList [length, (+ 1) . length] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [1, 2] :: ZipList Int),
          testCase "size > 1, size > 1, size = 0" $ assertLawComposition (ZipList [length, (+ 1) . length] :: ZipList (String -> Int)) (ZipList [show, (++ "test") . show] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size > 1, size > 1, size > 1" $ assertLawComposition (ZipList [length, (+ 1) . length] :: ZipList (String -> Int)) (ZipList [show, (++ "test") . show] :: ZipList (Int -> String)) (ZipList [1, 2] :: ZipList Int),
          -- mixed
          testCase "size > 1, size = 1, size = 0" $ assertLawComposition (ZipList [length, (+ 1) . length] :: ZipList (String -> Int)) (ZipList [show] :: ZipList (Int -> String)) (ZipList [] :: ZipList Int),
          testCase "size = 1, size = 0, size > 1" $ assertLawComposition (ZipList [length] :: ZipList (String -> Int)) (ZipList [] :: ZipList (Int -> String)) (ZipList [1, 2] :: ZipList Int),
          testCase "size = 0, size > 1, size = 1" $ assertLawComposition (ZipList [] :: ZipList (String -> Int)) (ZipList [show, (++ "test") . show] :: ZipList (Int -> String)) (ZipList [1] :: ZipList Int)
        ]
    ]

sequenceALSuite :: TestTree
sequenceALSuite =
  testGroup
    "sequenceAL"
    $ map
      (\(testName, input, expected) -> testCase testName $ sequenceAL input @?= expected)
      [ ("empty list", [] :: [Maybe Int], Just []),
        ("singleton just", [Just 1], Just [1]),
        ("singleton nothing", [Nothing] :: [Maybe Int], Nothing),
        ("all justs", [Just 1, Just 2, Just 3], Just [1, 2, 3]),
        ("all nothings", [Nothing, Nothing] :: [Maybe Int], Nothing),
        ("mixed maybes", [Just 2, Nothing, Just 1] :: [Maybe Int], Nothing)
      ]
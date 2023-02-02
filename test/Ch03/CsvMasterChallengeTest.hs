{-# LANGUAGE OverloadedStrings #-}

module Ch03.CsvMasterChallengeTest where

import Ch03.CsvMasterChallenge.Data (Gender (..), Name (..), Person (..), Stats (..))
import qualified Ch03.CsvMasterChallenge.Fold.FoldMap as FoldMap
import qualified Ch03.CsvMasterChallenge.Fold.Foldl as Foldl
import qualified Ch03.CsvMasterChallenge.Fold.SimpleFold as SimpleFold
import Data.List (map)
import Data.Monoid (Sum (..))
import Data.Time (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

suite :: TestTree
suite =
  testGroup
    "CsvMasterChallenge"
    [ testGroup
        "fold"
        $ map
          testFold
          [ ("Simple Fold", SimpleFold.run),
            ("Fold Map", FoldMap.run),
            ("Foldl", Foldl.run)
          ]
    ]

testFold :: (String, [Person] -> Stats) -> TestTree
testFold (name, fold) =
  testGroup
    name
    $ map
      (testRun fold)
      [ ( "empty",
          [],
          Stats 0 Nothing
        ),
        ( "single female",
          [Person (Name "F") Female (fromGregorian 2020 12 31)],
          Stats 1 (Just $ Person (Name "F") Female (fromGregorian 2020 12 31))
        ),
        ( "single male",
          [Person (Name "M") Male (fromGregorian 2020 12 31)],
          Stats 0 (Just $ Person (Name "M") Male (fromGregorian 2020 12 31))
        ),
        ( "all females, length > 1",
          [ Person (Name "F1") Female (fromGregorian 2020 12 30),
            Person (Name "F2") Female (fromGregorian 2020 12 31)
          ],
          Stats 2 (Just $ Person (Name "F1") Female (fromGregorian 2020 12 30))
        ),
        ( "all males, length > 1",
          [ Person (Name "M1") Male (fromGregorian 2020 12 30),
            Person (Name "M2") Male (fromGregorian 2020 12 31)
          ],
          Stats 0 (Just $ Person (Name "M1") Male (fromGregorian 2020 12 30))
        ),
        ( "all genders",
          [ Person (Name "F1") Female (fromGregorian 2020 12 28),
            Person (Name "M1") Male (fromGregorian 2020 12 29),
            Person (Name "M2") Male (fromGregorian 2020 12 30),
            Person (Name "F2") Female (fromGregorian 2020 12 31)
          ],
          Stats 2 (Just $ Person (Name "F1") Female (fromGregorian 2020 12 28))
        ),
        ( "same birthdates",
          [ Person (Name "F1") Female (fromGregorian 2020 12 31),
            Person (Name "F2") Female (fromGregorian 2020 12 31),
            Person (Name "M1") Male (fromGregorian 2020 12 31),
            Person (Name "M2") Male (fromGregorian 2020 12 31)
          ],
          Stats 2 (Just $ Person (Name "F1") Female (fromGregorian 2020 12 31))
        ),
        ( "oldest in first position",
          [ Person (Name "F1") Female (fromGregorian 2020 12 31),
            Person (Name "F2") Female (fromGregorian 2020 12 31),
            Person (Name "M1") Male (fromGregorian 2020 12 31),
            Person (Name "M2") Male (fromGregorian 2020 12 31)
          ],
          Stats 2 (Just $ Person (Name "F1") Female (fromGregorian 2020 12 31))
        ),
        ( "oldest in last position",
          [ Person (Name "F1") Female (fromGregorian 2020 12 31),
            Person (Name "F2") Female (fromGregorian 2020 12 31),
            Person (Name "M1") Male (fromGregorian 2020 12 31),
            Person (Name "M2") Male (fromGregorian 2020 12 30)
          ],
          Stats 2 (Just $ Person (Name "M2") Male (fromGregorian 2020 12 30))
        ),
        ( "oldest in middle position",
          [ Person (Name "F1") Female (fromGregorian 2020 12 31),
            Person (Name "F2") Female (fromGregorian 2020 12 31),
            Person (Name "M1") Male (fromGregorian 2020 12 30),
            Person (Name "M2") Male (fromGregorian 2020 12 31)
          ],
          Stats 2 (Just $ Person (Name "M1") Male (fromGregorian 2020 12 30))
        )
      ]
  where
    testRun fold (name, input, expected) = testCase name $ do
      fold input @?= expected
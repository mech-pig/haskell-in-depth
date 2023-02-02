module Ch03.Suite where

import Ch03.CsvMasterChallengeTest
import Test.Tasty (TestTree, testGroup)

suite :: TestTree
suite =
  testGroup
    "CsvMasterChallenge"
    [Ch03.CsvMasterChallengeTest.suite]
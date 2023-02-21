module Ch05.Suite where

import Ch05.ExpressionsTest
import Test.Tasty (TestTree, testGroup)

suite :: TestTree
suite =
  testGroup
    "Ch05"
    [Ch05.ExpressionsTest.suite]
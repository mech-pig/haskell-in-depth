module Ch05.Suite where

import Ch05.ExpressionsTest
import Ch05.RPNTest
import Test.Tasty (TestTree, testGroup)

suite :: TestTree
suite =
  testGroup
    "Ch05"
    [ Ch05.ExpressionsTest.suite,
      Ch05.RPNTest.suite
    ]
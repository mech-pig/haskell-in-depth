module Ch02.Typeclassopedia.Suite where

import qualified Ch02.Typeclassopedia.ApplicativeTest
import qualified Ch02.Typeclassopedia.FunctorTest
import Test.Tasty (TestTree, testGroup)

suite :: TestTree
suite =
  testGroup
    "Typeclassopedia"
    [ Ch02.Typeclassopedia.FunctorTest.suite,
      Ch02.Typeclassopedia.ApplicativeTest.suite
    ]
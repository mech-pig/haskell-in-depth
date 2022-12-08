module Ch01.Suite where

import qualified Ch01.VocabularyTest
import Test.Tasty (testGroup)

suite =
  testGroup
    "Ch01"
    [Ch01.VocabularyTest.suite]
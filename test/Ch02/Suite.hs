module Ch02.Suite where

import qualified Ch02.Typeclassopedia.Suite
import Test.Tasty (testGroup)

suite =
  testGroup
    "Ch02"
    [Ch02.Typeclassopedia.Suite.suite]
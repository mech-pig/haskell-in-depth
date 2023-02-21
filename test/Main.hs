module Main (main) where

import qualified Ch01.Suite
import qualified Ch02.Suite
import qualified Ch03.Suite
import qualified Ch05.Suite
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [ Ch01.Suite.suite,
        Ch02.Suite.suite,
        Ch03.Suite.suite,
        Ch05.Suite.suite
      ]
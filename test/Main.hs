module Main (main) where

import qualified Ch01.Suite
import qualified Ch02.Suite
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "main" [Ch01.Suite.suite, Ch02.Suite.suite]
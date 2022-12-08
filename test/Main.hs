module Main (main) where

import qualified Ch01.Suite
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Ch01.Suite.suite
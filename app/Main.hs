module Main where

import qualified Ch01.Hangman
import qualified Ch01.Vocabulary
import qualified Ch03.CsvMasterChallenge.Main
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["hangman"] -> Ch01.Hangman.main
    ["vocabulary", filePath] -> Ch01.Vocabulary.main filePath
    ["typeclassopedia"] -> putStrLn "tbd"
    ["csv-master-challenge", strategy, filePath] -> Ch03.CsvMasterChallenge.Main.main strategy filePath
    _ -> putStrLn "invalid args"

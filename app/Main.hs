module Main where

import qualified Ch01.Hangman
import qualified Ch01.Vocabulary
import qualified Ch03.CsvMasterChallenge.Main
import qualified Ch05.Protected
import qualified Ch05.StructLog
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["hangman"] -> Ch01.Hangman.main
    ["vocabulary", filePath] -> Ch01.Vocabulary.main filePath
    ["typeclassopedia"] -> putStrLn "tbd"
    ["csv-master-challenge", strategy, filePath] -> Ch03.CsvMasterChallenge.Main.main strategy filePath
    ["mts", "protected"] -> Ch05.Protected.main Nothing
    ["mts", "protected", password] -> Ch05.Protected.main $ Just password
    ["mts", "structlog", "ts"] -> Ch05.StructLog.main True
    ["mts", "structlog"] -> Ch05.StructLog.main False
    _ -> putStrLn "invalid args"

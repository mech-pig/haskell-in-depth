module Main where

import qualified Ch01.Hangman
import qualified Ch01.Vocabulary
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["hangman"] -> Ch01.Hangman.main
    ["vocabulary", filePath] -> Ch01.Vocabulary.main filePath
    ["typeclassopedia"] -> putStrLn "tbd"
    _ -> putStrLn "invalid args"

module Main (main) where

import qualified Ch03.CsvMasterChallengeBench
import Criterion (bgroup)
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Ch03"
        [ Ch03.CsvMasterChallengeBench.main
        ]
    ]
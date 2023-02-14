{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch03.CsvMasterChallengeBench where

import Ch03.CsvMasterChallenge.Data (Gender (..), Name (..), Person (..), Stats (..))
import qualified Ch03.CsvMasterChallenge.Fold.FoldMap as FoldMap
import qualified Ch03.CsvMasterChallenge.Fold.Foldl as Foldl
import qualified Ch03.CsvMasterChallenge.Fold.SimpleFold as SimpleFold
import Control.Monad (return)
import Criterion (Benchmark, bench, bgroup, whnf)
import Data.Time (addDays, fromGregorian)

main :: Benchmark
main =
  bgroup
    "CsvMasterChallenge"
    [foldBench]

foldBench :: Benchmark
foldBench = bgroup "Fold" $ mkFoldBench <$> [1000, 10000]

mkFoldBench :: Int -> Benchmark
mkFoldBench n = bgroup ("n=" <> show n) $ (\(name, fold) -> bench name $ whnf (fold . generateInput) n) <$> foldsUnderTest

foldsUnderTest :: [(String, [Person] -> Stats)]
foldsUnderTest =
  [ ("SimpleFold", SimpleFold.run),
    ("FoldMap", FoldMap.run),
    ("Foldl", Foldl.run)
  ]

generateInput :: Int -> [Person]
generateInput n = take n $ iterate bornTheDayBefore $ Person (Name "F") Female (fromGregorian 2020 12 31)
  where
    bornTheDayBefore Person {name, gender, birthDate} = Person name gender (addDays (-1) birthDate)

{-# LANGUAGE NoImplicitPrelude #-}

module Ch03.CsvMasterChallenge.Main where

import Ch03.CsvMasterChallenge.Data (decodePerson)
import qualified Ch03.CsvMasterChallenge.Fold.FoldMap as FoldMap
import qualified Ch03.CsvMasterChallenge.Fold.Foldl as Foldl
import qualified Ch03.CsvMasterChallenge.Fold.SimpleFold as SimpleFold
import Control.Applicative (pure)
import Control.Monad.Fail (fail)
import Data.ByteString.Lazy (readFile)
import Data.Csv (HasHeader (..))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.String (String)
import Prelude (IO, print, ($))

main :: String -> String -> IO ()
main strategy filePath = do
  fold <- case strategy of
    "foldl" -> pure Foldl.run
    "foldMap" -> pure FoldMap.run
    "simple" -> pure SimpleFold.run
    _ -> fail $ "unknown strategy: " <> strategy
  fileContent <- readFile filePath
  let stats = fold <$> decodePerson NoHeader fileContent
  print stats

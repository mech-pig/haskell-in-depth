{-# LANGUAGE NoImplicitPrelude #-}

module Ch03.CsvMasterChallenge.Fold.Foldl where

import Ch03.CsvMasterChallenge.Data (Gender (..), Person (..), Stats (..))
import Control.Applicative ((<*>))
import Control.Foldl (Fold (..), fold, minimumBy)
import Data.Bool (Bool)
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Ord (comparing)
import Prelude (fromEnum, id, ($), (+), (.), (==))

run :: Foldable f => f Person -> Stats
run = (\(f, o) -> Stats f o) . fold ((,) <$> countFemales <*> oldest)

countIf :: (a -> Bool) -> Fold a Int
countIf pred = Fold (\t b -> (+ t) . fromEnum . pred $ b) 0 id

countFemales :: Fold Person Int
countFemales = countIf ((== Female) . gender)

oldest :: Fold Person (Maybe Person)
oldest = minimumBy (comparing birthDate)
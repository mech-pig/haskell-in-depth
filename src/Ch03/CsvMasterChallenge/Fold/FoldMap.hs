{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch03.CsvMasterChallenge.Fold.FoldMap where

import Ch03.CsvMasterChallenge.Data (Gender (..), Person (..), Stats (..))
import Data.Either (Either (..))
import Data.Foldable (Foldable, foldMap)
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (..), Sum (..))
import Data.Semigroup (Semigroup ((<>)))
import Prelude ((.), (<=))

run :: Foldable f => f Person -> Stats
run = getOutput . foldMap step
  where
    step p =
      MonoidStats
        { femalesCount =
            Sum
              ( case gender p of
                  Female -> 1
                  _ -> 0
              ),
          oldest = Oldest (Just p)
        }

    getOutput :: MonoidStats -> Stats
    getOutput MonoidStats {femalesCount, oldest} = Stats {statsFemalesCount = getSum femalesCount, statsOldestPerson = getOldest oldest}

data MonoidStats = MonoidStats {femalesCount :: Sum Int, oldest :: Oldest}

newtype Oldest = Oldest {getOldest :: Maybe Person}

instance Semigroup Oldest where
  o <> (Oldest Nothing) = o
  (Oldest Nothing) <> o = o
  o1@(Oldest (Just p1)) <> o2@(Oldest (Just p2)) =
    if birthDate p1 <= birthDate p2
      then o1
      else o2

instance Monoid Oldest where
  mempty = Oldest Nothing

instance Semigroup MonoidStats where
  s1 <> s2 = MonoidStats (femalesCount s1 <> femalesCount s2) (oldest s1 <> oldest s2)

instance Monoid MonoidStats where
  mempty = MonoidStats mempty mempty
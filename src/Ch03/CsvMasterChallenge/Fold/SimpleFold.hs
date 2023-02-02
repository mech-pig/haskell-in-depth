{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch03.CsvMasterChallenge.Fold.SimpleFold where

import Ch03.CsvMasterChallenge.Data (Gender (..), Person (..), Stats (..))
import Data.Foldable (Foldable, foldr)
import Data.Maybe (Maybe (..))
import Prelude ((+), (<=))

run :: Foldable f => f Person -> Stats
run = foldr step init
  where
    step :: Person -> Stats -> Stats
    step p Stats {statsFemalesCount, statsOldestPerson} =
      Stats
        { statsFemalesCount =
            statsFemalesCount + case gender p of
              Female -> 1
              _ -> 0,
          statsOldestPerson =
            case statsOldestPerson of
              Nothing -> Just p
              (Just p2) -> Just (if birthDate p <= birthDate p2 then p else p2)
        }

    init :: Stats
    init = Stats {statsFemalesCount = 0, statsOldestPerson = Nothing}
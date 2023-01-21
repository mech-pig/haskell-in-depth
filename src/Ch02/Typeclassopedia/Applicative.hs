{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.Applicative where

import Ch02.Typeclassopedia.Data (Maybe (..), ZipList (..))
import Ch02.Typeclassopedia.Functor ()
import Control.Applicative (Applicative (pure, (<*>)))
import Data.Foldable (foldr)
import Data.Functor ((<$>))
import Data.List (repeat, zipWith)
import GHC.Base (undefined, ($))
import Test.Tasty.HUnit (testCase, (@=?), (@?=))

-- implement an instance of Applicative for Maybe
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) (Just f) (Just a) = Just (f a)
  (<*>) _ _ = Nothing

-- Determine the correct definition of pure for the ZipList instance of Applicative;
-- there is only one implementation that satisfies the law relating pure and (<*>).
instance Applicative ZipList where
  pure :: a -> ZipList a
  pure a = ZipList (repeat a)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (<*>) (ZipList funcs) (ZipList as) = ZipList (zipWith ($) funcs as)

-- Implement a function `sequenceAL :: Applicative f => [f a] -> f [a]`
-- There is a generalized version of this, sequenceA, which works for any Traversable (see the later section on Traversable), but implementing this version specialized to lists is a good exercise.
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL = foldr (\fa acc -> (:) <$> fa <*> acc) (pure [])
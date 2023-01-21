{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.Functor where

import Ch02.Typeclassopedia.Data
  ( Either (..),
    FunctionApplication (..),
    ITree (..),
    Maybe (..),
    Pair (..),
    Tuple (..),
    ZipList (..),
  )
import Data.Bool (Bool (True))
import Data.Eq (Eq ((==)))
import Data.Function ((.))
import Data.Functor (Functor, fmap)
import Data.List (map)

-- implement Functor instance for Maybe
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just a) = Just (f a)
  fmap _ _ = Nothing

-- implement Functor instance for Either e
instance Functor (Either e) where
  fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Right a) = Right (f a)
  fmap f (Left e) = Left e

-- -- implement Functor instance for ((->) r)
instance Functor (FunctionApplication r) where
  fmap :: (a -> b) -> FunctionApplication r a -> FunctionApplication r b
  -- fmap f (FunctionApplication fa) = FunctionApplication (\r -> f (fa r)) -- equivalent
  fmap f (FunctionApplication fa) = FunctionApplication (f . fa)

-- implement Functor instance for ((,), e)
instance Functor (Tuple e) where
  fmap :: (a -> b) -> Tuple e a -> Tuple e b
  fmap f (Tuple (e, a)) = Tuple (e, f a)

-- implement Functor instance for Pair
instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair (a0, a1)) = Pair (f a0, f a1)

-- implement Functor instance for ITree
instance Functor ITree where
  fmap :: (a -> b) -> ITree a -> ITree b
  fmap f (Leaf l) = Leaf (f . l)
  fmap f (Node ts) = Node (map (fmap f) ts)

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f zl = ZipList (fmap f (unZipList zl))

-- -- Give an example of a type of kind `* -> *`
-- -- which cannot be made an instance of Functor
-- -- (without using undefined)

-- -- is the composition of two Functors a Functor?

-- -- Laws:
-- -- identity := fmap id = id -- mapping the identity function over every item in the container has not effect
-- -- composition := fmap (g . f) = (fmap g) . (fmap f) -- mapping a composition of two functions is the same as mapping the first function and then the second
-- -- the identity law implies the composition law, but the reverse is not true

-- -- give an example of a (bogus) Functor instance which satisfies the second law but not the first
-- newtype Bogus a = Bogus (a, Bool)

-- instance Functor Bogus where
--   fmap :: (a -> b) -> Bogus a -> Bogus b
--   fmap f Bogus (a, _) = Bogus (f a, True)

-- -- which laws are violated by this Evil Fuctor instance?
-- instance Functor [] where
--   fmap :: (a -> b) -> [a] -> [b]
--   fmap _ [] = []
--   fmap g (x : xs) = g x : g x : fmap g xs

-- -- both identity and composition are violated as any application of
-- -- fmap duplicate the head of the list:
-- --  fmap id (x:xs) = x:x:xs != id (x:xs) = x:xs
-- --  fmap (id . id) (x:xs) = fmap id (x:xs) = x:x:xs != ((fmap id) . (fmap id)) (x:xs) = (fmap id (fmap id (x:xs))) = fmap id (x:x:xs) = x:x:x:xs

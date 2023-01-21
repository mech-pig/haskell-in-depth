{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.Data where

import Data.Eq (Eq)
import Data.Int (Int)
import Text.Show (Show)

data Maybe a = Nothing | Just a deriving (Eq, Show)

data Either e a = Left e | Right a deriving (Eq, Show)

-- equivalent to (->) r a; newtype to avoid error due to illegal binding of built-in syntax
newtype FunctionApplication r a = FunctionApplication (r -> a)

apply :: FunctionApplication r a -> r -> a
apply (FunctionApplication func) = func

-- equivalent to (,) a b; newtype to avoid error due to illegal binding of built-in syntax
newtype Tuple a b = Tuple (a, b) deriving (Eq, Show)

-- equivalent to (a, a); newtype to avoid error due to illegal binding of built-in syntax
newtype Pair a = Pair (a, a) deriving (Eq, Show)

data ITree a = Leaf (Int -> a) | Node [ITree a]

newtype ZipList a = ZipList {unZipList :: [a]} deriving (Eq, Show)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ch02.Typeclassopedia.TestUtils where

import Data.Eq (Eq)
import GHC.Base (($))
import Test.Tasty.HUnit (Assertion, HasCallStack, (@?=))
import Text.Show (Show)

newtype TypedAssertion f = TypedAssertion {runTypedAssertion :: Assertion}

(@?=*) :: forall f a. (Eq a, Show a, HasCallStack) => a -> a -> Assertion
(@?=*) actual expected = runTypedAssertion $ TypedAssertion @f (actual @?= expected)

infix 1 @?=*
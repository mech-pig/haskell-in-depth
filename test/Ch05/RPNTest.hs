{-# LANGUAGE OverloadedStrings #-}

module Ch05.RPNTest where

import Ch05.RPN
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

suite :: TestTree
suite =
  testGroup
    "RPN"
    $ map
      ( \(testName, expr, result) ->
          testCase testName $ Ch05.RPN.eval expr @?= result
      )
      [ ("single integer", "2", Just 2),
        ("addition", "3 2 +", Just 5),
        ("subtraction", "2 3 -", Just (-1)),
        ("multiple computations", "2 4 + 3 4 + *", Just 42),
        ("single unsupported token", "a", Nothing),
        ("unsupported token in computation", "1 a +", Nothing),
        ("multiple integers", "4 5", Nothing),
        ("unbalanced computation - single operator", "+", Nothing),
        ("unbalanced computation - missing operand", "4 +", Nothing)
      ]
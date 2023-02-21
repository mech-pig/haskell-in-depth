{-# LANGUAGE OverloadedStrings #-}

module Ch05.ExpressionsTest where

import Ch05.Expressions
import Control.Monad (mapM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

suite :: TestTree
suite =
  testGroup
    "Expressions"
    $ map
      ( \(testName, expr, result) ->
          testGroup
            testName
            $ map
              (\parser -> testCase parser $ Ch05.Expressions.run parser expr @?= Right result)
              ["parsec"]
      )
      [ ("literal int - single digit", "3", 3),
        ("literal int - multiple digits", "56", 56),
        ("literal int - surrounding spaces", "  38\n", 38),
        ("literal int - parentheses", "(21)", 21),
        ("literal int - parentheses with surrounding spaces", "   ( 12   ) ", 12),
        ("add - simple", "1+2", 3),
        ("add - parenthesis", "(1+2)", 3),
        ("add - spaces", "  1 +\n2    ", 3),
        ("add - spaces, parenthesis", "  ((((  1)) +\n(2)))    ", 3),
        ("add - multiple operations - even", "15 + 2 + 10 + 3", 30),
        ("add - multiple operations - odd", "15 + 2 + 10", 27),
        ("mult - simple", "15*10", 150),
        ("mult - parenthesis", "(15*10)", 150),
        ("mult - spaces", "  15 *\n10    ", 150),
        ("mult - spaces, parenthesis", "  ((((  15)) *\n(10)))    ", 150),
        ("mult - multiple operations - even", "15 * 2 * 10 * 3", 900),
        ("mult - multiple operations - odd", "15 * 2 * 10", 300),
        ("precedence - no parens, add first", "2+3*5", 17),
        ("precedence - no parens, mult first", "2*3+5", 11),
        ("precedence - parens, add first", "(2+3)*5", 25),
        ("precedence - parens, mult first", "2*(3+5)", 16)
      ]
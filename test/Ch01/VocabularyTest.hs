{-# LANGUAGE OverloadedStrings #-}

module Ch01.VocabularyTest where

import Ch01.Vocabulary (WordFrequencies, countWordFrequencies, normalizeWord, topWords)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

suite :: TestTree
suite =
  testGroup
    "Vocab - Ex. 01"
    [ normalizeWordTest,
      countWordFrequenciesTest,
      topWordsTest
    ]

normalizeWordTest :: TestTree
normalizeWordTest =
  testGroup
    "normalizeWord"
    $ map
      createTestCase
      [ ("word is already normalized", "word", "word"),
        ("leading spaces are removed", "   test", "test"),
        ("trailing spaces are removed", "test   ", "test"),
        ("leading and trailing spaces are removed", " test   ", "test"),
        ("text is lowercased", "MiXEdCASe", "mixedcase"),
        ("leading and trailing punctuation is removed", ".!!!?test-with/p_u.n.c\\tuat!?on-!./", "test-with/p_u.n.c\\tuat!?on"),
        ("digits are not removed", "0123test0987", "0123test0987")
      ]
  where
    createTestCase :: (String, T.Text, T.Text) -> TestTree
    createTestCase (testName, input, expected) = testCase testName $ do
      expected @=? normalizeWord input

countWordFrequenciesTest :: TestTree
countWordFrequenciesTest =
  testGroup
    "countWordFrequencies"
    $ map
      createTestCase
      [ ("empty list", [] :: [T.Text], HM.empty :: WordFrequencies),
        ("single word", ["test"], HM.fromList [("test", 1)]),
        ("multiple singleton words", ["one", "two", "three"], HM.fromList [("one", 1), ("two", 1), ("three", 1)]),
        ("single word with multiple occurrences", ["one", "one", "one"], HM.fromList [("one", 3)]),
        ("multiple words with multiple occurrences", ["one", "three", "one", "two", "two", "one"], HM.fromList [("one", 3), ("two", 2), ("three", 1)])
      ]
  where
    createTestCase :: (String, [T.Text], WordFrequencies) -> TestTree
    createTestCase (testName, input, expected) = testCase testName $ do
      expected @=? countWordFrequencies input

topWordsTest :: TestTree
topWordsTest =
  testGroup
    "topWords"
    $ map
      createTestCase
      [ ("words are sorted according to decreasing frequencies", (3, HM.fromList [("a", 2), ("c", 3), ("b", 1)]), [("c", 3), ("a", 2), ("b", 1)]),
        ("ties are solved using lexicographical order", (1, HM.fromList [("b", 2), ("a", 2)]), [("a", 2)]),
        ("top 0 - dictionary size = 0", (0, HM.empty), []),
        ("top 1 - dictionary size = 0", (1, HM.empty), []),
        ("top N > 1 - dictionary size = 0", (3, HM.empty), []),
        ("top 0 - dictionary size = 1", (0, HM.fromList [("a", 1)]), []),
        ("top 1 - dictionary size = 1", (1, HM.fromList [("a", 2)]), [("a", 2)]),
        ("top N > 1 - dictionary size = 1", (3, HM.fromList [("a", 3)]), [("a", 3)]),
        ("top 0 - dictionary size > 1", (0, HM.fromList [("b", 1), ("a", 2)]), []),
        ("top 1 - dictionary size > 1", (1, HM.fromList [("b", 1), ("a", 2)]), [("a", 2)]),
        ("top N > 1 - dictionary size = N", (2, HM.fromList [("b", 1), ("a", 2)]), [("a", 2), ("b", 1)]),
        ("top N > 1 - dictionary size > N", (2, HM.fromList [("b", 2), ("a", 3), ("c", 1)]), [("a", 3), ("b", 2)]),
        ("top N > 1 - dictionary size > 1, < N", (3, HM.fromList [("b", 2), ("a", 3)]), [("a", 3), ("b", 2)])
      ]
  where
    createTestCase :: (String, (Int, WordFrequencies), [(T.Text, Int)]) -> TestTree
    createTestCase (testName, (topN, wordFreqs), expected) = testCase testName $ do
      expected @=? topWords topN wordFreqs
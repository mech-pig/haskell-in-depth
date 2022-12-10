{-# LANGUAGE OverloadedStrings #-}

module Ch01.Vocabulary
  ( main,
    countWordFrequencies,
    normalizeWord,
    WordFrequencies,
    topWords,
  )
where

import Data.Char (isAlphaNum)
import qualified Data.HashMap.Lazy as HM
import Data.List (group, sort, sortBy)
import Data.Ord (Ordering)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: String -> IO ()
main filePath = do
  log ("reading file: " <> T.pack filePath)
  text <- TIO.readFile filePath
  log ("extracting top " <> T.pack (show n) <> " words")
  topN <- extractTopNWords log n text
  log ("result: " <> T.pack (show topN))
  where
    log :: Logger
    log = T.unpack |> putStrLn

    n :: Int
    n = 10

type Tokenizer = T.Text -> [T.Text]

type Stemmer = T.Text -> T.Text

type WordFrequencies = HM.HashMap T.Text Int

type WordFrequency = (T.Text, Int)

type FrequencyCounter = [T.Text] -> WordFrequencies

type Logger = T.Text -> IO ()

extractTopNWords :: Logger -> Int -> T.Text -> IO [WordFrequency]
extractTopNWords = mkTopWordsService T.words normalizeWord countWordFrequencies

mkTopWordsService :: Tokenizer -> Stemmer -> FrequencyCounter -> Logger -> Int -> T.Text -> IO [WordFrequency]
mkTopWordsService tokenize stem countFreq log n text = do
  log "starting stemming"
  stems <- tokenize |> traverse (\t -> stem |> passThrough (\s -> log ("extracted stem \"" <> s <> "\" from token \"" <> t <> "\"")) $ t) $ text
  log "starting frquency count"
  let wordFreqs = countFreq stems
  log ("extracting top " <> T.pack (show n) <> " words")
  topWords n |> pure $ wordFreqs

uniqueWords :: T.Text -> WordFrequencies
uniqueWords = T.words |> map normalizeWord |> countWordFrequencies

normalizeWord :: T.Text -> T.Text
normalizeWord = T.toLower |> T.dropAround (not . isAlphaNum)

countWordFrequencies :: [T.Text] -> WordFrequencies
countWordFrequencies = foldr increaseFreq HM.empty
  where
    increaseFreq :: T.Text -> WordFrequencies -> WordFrequencies
    increaseFreq word = HM.insertWith (+) word 1

topWords :: Int -> WordFrequencies -> [(T.Text, Int)]
topWords n = HM.toList |> sortBy compareEntry |> take n
  where
    compareEntry :: (T.Text, Int) -> (T.Text, Int) -> Ordering
    compareEntry (wordFst, freqFst) (wordSnd, freqSnd)
      | freqFst == freqSnd = compare wordFst wordSnd
      | otherwise = compare freqSnd freqFst

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f g = g . f

passThrough :: Applicative m => (b -> m a) -> b -> m b
passThrough f = (*>) <$> f <*> pure
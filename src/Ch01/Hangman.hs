{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Ch01.Hangman (main) where

import qualified Data.HashSet as HashSet
import Data.List (elem, (!!))
import System.Random (getStdRandom, randomR)

main :: IO ()
main = do
  log "Hangman - Haskell in Depth Edition"
  target <- getRandomWord
  runGame 2 target log
  where
    log = putStrLn

wordList :: [String]
wordList =
  [ "prova",
    "ouch"
  ]

runGame :: MaxMisses -> Target -> Logger -> IO ()
runGame maxMisses target log = runTurn newGameState
  where
    runTurn :: GameState -> IO ()
    runTurn gameState = do
      let gameStatus = status maxMisses target gameState
      case gameStatus of
        Won -> log "You won"
        Lost -> log "You lost"
        WaitingForInput -> do
          let (guesses, missCount) = gameState
          log $ "\ntarget: " ++ maskTarget target guesses
          log $ "remaining tentatives: " ++ show (maxMisses - missCount)
          log "guess a letter: "
          char : _ <- getLine -- TODO: handle wrong input
          let (updateGameState, msg) = guess target char gameState
          case msg of
            Miss -> log "nope!"
            Hit -> log "bingo!"
            InvalidInput -> log "invalid"
          runTurn updateGameState

getRandomWord :: IO String
getRandomWord = (wordList !!) <$> getStdRandom (randomR (0, length wordList - 1))

type Logger = String -> IO ()

data Msg = Miss | Hit | InvalidInput

data GameStatus = WaitingForInput | Won | Lost

type Target = String

type MaxMisses = Int

type MissCount = Int

type Guesses = HashSet.HashSet Char

type GameState = (Guesses, MissCount)

newGameState :: GameState
newGameState = (HashSet.empty, 0)

status :: MaxMisses -> Target -> GameState -> GameStatus
status maxMisses target (guesses, missCount)
  | missCount > maxMisses = Lost
  | HashSet.isSubsetOf (HashSet.fromList target) guesses = Won
  | otherwise = WaitingForInput

guess :: Target -> Char -> GameState -> (GameState, Msg)
guess target char (guesses, missCount)
  | HashSet.member char guesses = ((guesses, missCount), InvalidInput)
  | char `elem` target = ((HashSet.insert char guesses, missCount), Hit)
  | otherwise = ((guesses, missCount + 1), Miss)

maskTarget :: Target -> Guesses -> String
maskTarget target guesses = map (\c -> if HashSet.member c guesses then c else '_') target

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f g = g . f
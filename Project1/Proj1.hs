{-|
Module      : Proj1
Description : Module that implements functions that efficiently guess the 
              value of a pre-determined random chord, in the game of 
              ChordProbe. This module was primarily written for Declarative
              Programming, Semester-2 2017. 
Author      : Shreyash Patodia
Maintainer  : spatodia@student.unimelb.edu.au
Stability   : stable
-}
module  Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Function
import Data.Ord

type Pitch = String
type Chord = [Pitch]
type GameState = [Chord]

{-|
  The generateGuesses function generates all possible Pitches that can
  exist given our namespaces for Notes and Octaves. 
-}
generateGuesses :: [Pitch]
generateGuesses = [ (x:y:[]) | 
    x <- ['A'..'G'], y <- ['1'..'3']]


{-|
  The choose function produces all the ways you can choose n elements
  from a list xs of size say k. This allows me to 
-}
choose :: [Pitch] -> Int -> [Chord]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\rs -> ([x] ++ rs)) (choose xs (n-1))) 
          ++ (choose xs n)

generateGameStates :: GameState
-- Find a fix for the 3
generateGameStates = choose generateGuesses chordLen
    where chordLen = 3

-- Best first guess was found by running my nextGuess function on the
-- initial sample space
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B1", "C2"], gs) 
    where gs = generateGameStates

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess metadata feedback = (newGuess, newGS)
    where newGS = reduceGameState metadata feedback
          newGuess = makeGuess newGS
    --- (makeGuess reducedGS, reducedGS)
    --- where reducedGS = reduceGameState (prevGuess, gs) feedback


reduceGameState :: (Chord, GameState) -> (Int, Int, Int) -> GameState
reduceGameState (prevGuess, gs) feedback = 
          [x | x <- gs, (produceFeedback x prevGuess) == feedback]

produceFeedback :: Chord -> Chord -> (Int, Int, Int)
produceFeedback target guess = (correct, correctNote, correctOct)
    where chordLen = length guess 
          correct  = chordLen - length (target \\ guess)
          correctNote = chordLen - correct - (incorrectNotes target guess)
          correctOct = chordLen - correct - (incorrectOctaves target guess)

incorrectNotes :: Chord -> Chord -> Int
incorrectNotes target guess = length (tNotes \\ gNotes)
    where tNotes = map head target
          gNotes = map head guess

incorrectOctaves :: Chord -> Chord -> Int
incorrectOctaves target guess = length (tOctaves \\ gOctaves)
    where tOctaves = map last target
          gOctaves = map last guess

-- needs checking
simulateGuessResults :: GameState -> Chord -> Float
simulateGuessResults gs guess = average
    where possibleOutcomes = sort $ [produceFeedback x guess | x <- gs]
          totalOutcomes = fromIntegral $ length possibleOutcomes
          groupedFeedBack = group possibleOutcomes
          sumSetSizes = sum [length x ^ 2 | x <- groupedFeedBack]
          average = (fromIntegral sumSetSizes) / totalOutcomes
          
                             
makeGuess :: GameState -> Chord
makeGuess gs = fst $ minimumBy (comparing snd) chordValuePair
    where avgValues = map (simulateGuessResults gs) gs
          chordValuePair = zip gs avgValues
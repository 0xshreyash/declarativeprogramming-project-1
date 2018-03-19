{-|
Module      : Proj1
Project     : Project-1
Summary     : ChordProbe game solver implemented for comp30020,
              depends on Proj1Test in order to run.
Subject     : Declarative Programming, COMP30020.
Author      : Shreyash Patodia, spatodia, 767336
Maintainer  : spatodia@student.unimelb.edu.au
Stability   : stable, tested against all possible inputs.
-}

{--|
Description : Module that implements functions that efficiently guess the 
              value of a pre-determined random chord, in the game of 
              ChordProbe. This module was primarily written for Declarative
              Programming, Semester-2 2017. The function initialGuess makes
              an initial guess for the game, nextGuess makes the next guess
              give the current state of the game and the GameState is this
              programs internal representation of the game.
              Chord probe asks the users to guess a un-ordered target of
              the form (x, y, z) where each of x, y and z are made of
              a letter (A..G) and a number (1..3).
        
--}

-- In this file gs is used as an abbreviation for game state for breivity --
------------- The above mainly applies to variable names -------------------

module  Proj1 (initialGuess, nextGuess, GameState) where

-- Imported libraries
import Data.List
import Data.Function
import Data.Ord

-- Pitch is just an array of two Chars i.e. a String
type Pitch = String
-- Chord is a collection of 3 pitches i.e. an array of Pitches
type Chord = [Pitch]
-- The GameState is just a collection of chords. 
type GameState = [Chord]

----------------------- Initial Guess Functions ----------------------------

{--|
  The function returns the best first guess, the value is set to
  the guess that guarantees the best initial game state reduction
  when using simulateGuessResults but the function is not called
  here since it is very computationally expensive and the answer
  will always be the same. 
--}
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B1", "C2"], gs) 
    where gs = generateGameStates

{-|
  The generatePitches function generates all possible Pitches that can
  exist given our namespaces for Notes and Octaves. 
-}
generatePitches :: [Pitch]
generatePitches = [ (x:y:[]) | 
    x <- ['A'..'G'], y <- ['1'..'3']]

{--|
  The function generates all pitches and then uses the chordLen
  to produce all possible chords, if the possible Pitches may
  chage then just replace the generatePitches function, chordLen
  can be changed if needed. 
-}
generateGameStates :: GameState
generateGameStates = choose generatePitches chordLen
    where chordLen = 3

{-|
  The choose function produces all the ways you can choose n elements
  from a list xs of size say k. This allows us to choose 3 things out
  of a list of n things.
-}
choose :: [Pitch] -> Int -> [Chord]
-- If we want to pick 0 things, there is only one way, and there are
-- no ways to pick something from an empty list
choose _ 0 = [[]]
choose [] _ = []
-- All possiblities = possibilities that include x + the ones that don't
choose (x:xs) n = (map (\rs -> ([x] ++ rs)) (choose xs (n-1))) 
          ++ (choose xs n)

----------------------- Next Guess Functions ---------------------------

{--|
  The function takes the previous guess, gamestate and feedback from
  the previous guess and produces a new guess, the metadata is the
  previous guess and the GameState (since it is data about the game).
  We first reduce the game state, using the feedback and the previous
  state (valid options should produce the same feedback as the one
  returned) and then use that to simulate results and pick the best
  guess (more on this in the functions below)
--}
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess metadata feedback = (newGuess, newGS)
    where newGS = reduceGameState metadata feedback
          newGuess = makeGuess newGS

{--|
  The function computes the reduced game state by using the feedback from
  the previous guess, to see which elements of the game state do not
  produce the same feedback and thus aren't possible solutions
--}
reduceGameState :: (Chord, GameState) -> (Int, Int, Int) -> GameState
reduceGameState (prevGuess, gs) feedback = 
          [x | x <- gs, (produceFeedback x prevGuess) == feedback]

{--|
  The function takes a two chords, one target and one guess and produces
  the feedback in the same way the game would produce if the first chord
  was the target and the second the guess
--}
produceFeedback :: Chord -> Chord -> (Int, Int, Int)
produceFeedback target guess = (correct, correctNote, correctOct)
    where chordLen = length guess 
          correct  = chordLen - length (target \\ guess)
          -- need to make sure to not include the correct prediction in the
          -- no. of correct notes and chords so take away correct from both. 
          correctNote = chordLen - correct - (incorrectNotes target guess)
          correctOct = chordLen - correct - (incorrectOctaves target guess)

{--|
  The function takes two chords, and finds how many notes are different
  between them.
--}
incorrectNotes :: Chord -> Chord -> Int
incorrectNotes target guess = length (tNotes \\ gNotes)
    where tNotes = map head target
          gNotes = map head guess

{--|
  The function takes two chords, and finds how many octaves are different
  between them. The two functions above could be more generalised and I had
  that in my solution but changed since the lecturer had done the same thing
  as me, and I didn't know how to do it otherwise. 
--}
incorrectOctaves :: Chord -> Chord -> Int
incorrectOctaves target guess = length (tOctaves \\ gOctaves)
    where tOctaves = map last target
          gOctaves = map last guess

{--|
  Given a gamestate and a guess, it picks every element in
  the gamestate as the target in order to produce feedback, it then
  counts how many times each feedback occured and the expected size of
  the gs if we used this guess as our next guess
--}
simulateGuessResults :: GameState -> Chord -> Float
simulateGuessResults gs guess = average
          -- sort so that all of instances of the same feedback are
          -- close to each other and group-able i.e. a, b, a turns
          -- into a, a, b
    where possibleOutcomes = sort $ [produceFeedback x guess | x <- gs]
          totalOutcomes = fromIntegral $ length possibleOutcomes
          groupedFeedBack = group possibleOutcomes
          -- Calculate avg. = x * x/totalOutcomes
          sumSetSizes = sum [length x ^ 2 | x <- groupedFeedBack]
          average = (fromIntegral sumSetSizes) / totalOutcomes
          
{--|
  Simulates each game state value to be the guess, gets the expected size
  of the game state for each and then takes the smallest 
  (one with the smallest) expected game state size as the best guess
--} 
makeGuess :: GameState -> Chord
makeGuess gs = fst $ minimumBy (comparing snd) chordValuePair
    where avgValues = map (simulateGuessResults gs) gs
          chordValuePair = zip gs avgValues
---------------------------------------------------------------------------
                ---- !Haskell is beautiful! -----

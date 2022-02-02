module Main where

import LetterCounterGuesser
import Game
import GameTest
import Text.Printf

main :: IO ()
-- main = findValidWords >>= print
-- main = startGame

main = do
  answerSpace <- wordleAnswerSpace
  guessSpace <- wordleGuessSpace
  let guesser = findValidWords (answerSpace ++ guessSpace)
  -- let guesser = findValidWords answerSpace
  -- let answerSpaceReduced = take 10 answerSpace
  -- print answerSpaceReduced
  -- print $ startTest answerSpaceReduced guesser
  print $ getStats $ startTest (answerSpace ++ guessSpace) guesser

wordleAnswerSpace :: IO [String]
wordleAnswerSpace = words <$> readFile "valid_solutions.csv"

wordleGuessSpace :: IO [String]
wordleGuessSpace = words <$> readFile "valid_guesses.csv"

getStats :: [(Int,Int)] -> [(Int,Int,String)]
getStats counts = map (\(n,c) -> (n,c, roundToStr 3 ((fromIntegral c :: Float) / (fromIntegral s :: Float) * 100) ++ "%" )) counts
  where
    s = sum $ map snd counts

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr n = printf (printf "%%0.%df" n)
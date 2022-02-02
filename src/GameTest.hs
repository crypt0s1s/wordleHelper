{-# LANGUAGE MultiWayIf #-}

module GameTest ( startTest )
  where

import Control.Monad.State.Lazy
import qualified Data.MultiSet as MS
import qualified Data.Map as M
import Data.List

-- Note the guesser type will have the solution space dictionary and the guess space allready given to it. This is a partially applied function
type Guesser = (Words -> String)
data GameTest = GameTest
  {
    answer :: String,
    count :: Int,
    guesser :: Guesser,
    aSpace :: Words
  }

type WinTurnCount = MS.MultiSet Int
type Words = [String]
type PositionMap = M.Map Int String
data Restrictions = Restrictions
  {
    notIncluded :: String,
    includedAtPos :: PositionMap,
    includedNotAtPos :: PositionMap
  }

startTest :: Words -> Guesser -> [(Int,Int)]
startTest answerSpace guesser = MS.toOccurList $ MS.fromList $ map (startGame guesser answerSpace) answerSpace

startGame :: Guesser -> Words -> String -> Int
startGame guesser answerSpace solution = evalState playGame game
  where
    game = initialiseGame guesser answerSpace solution

initialiseGame :: Guesser -> Words -> String -> GameTest
initialiseGame guesser answerSpace solution = GameTest solution 1 guesser answerSpace

playGame :: State GameTest Int
playGame = do
  game <- get
  let ans = answer game
  let suggestedWord = guesser game (aSpace game)
  if suggestedWord == ans
    then
      return $ count game
    else do
      updateAnswerSpace suggestedWord
      playGame

updateAnswerSpace :: String -> State GameTest ()
updateAnswerSpace guess = do
  game <- get
  let ans = answer game
  let conditions = generateConditions guess ans
  let newAnswerSpace = filter conditions (aSpace game)
  let game' = game { count = count game + 1, aSpace = newAnswerSpace }
  put game'

generateConditions :: String -> String -> (String -> Bool)
generateConditions guess answer = \s -> c1 s && c2 s && c3 s
  where
    guess' = zip guess [0..]
    answer' = zip answer [0..]
    (correctPos,incorrectPos) = partition (uncurry (==)) $ zip guess' answer'
    c1 str = all ((\(c,pos) -> str !! pos == c) . fst) correctPos
    (inWord,notInWord) = partition (\(c,_) -> c `elem` answer) $ map fst incorrectPos
    c2 str = all (\(c,pos) -> str !! pos /= c && c `elem` str) inWord
    c3 str = all (\(c,_) -> c `notElem` str) notInWord

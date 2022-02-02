module LetterCounterGuesser ( findValidWords, findValidWordsWRes )
  where

import Data.List.Split ( splitOn )
import System.Environment (getArgs)
import Data.List ( nub )
import qualified Data.MultiSet as MS
import qualified Data.Map as M

type CountSet = MS.MultiSet Char
type Words = [String]
type PositionMap = M.Map Int String

data Restrictions = Restrictions
  {
    notIncluded :: String,
    includedAtPos :: PositionMap,
    includedNotAtPos :: PositionMap
  }

findValidWords :: Words -> Words -> String
findValidWords _ = recomendedGuesser

recomendedGuesser :: Words -> String
recomendedGuesser ws = snd $ maximum $ zip (map (getWordValue counts) ws) ws
  where
    counts = foldr (\w m -> foldr MS.insert m $ nub w) MS.empty ws
    getWordValue :: CountSet -> String -> Int
    getWordValue s w = sum $ map (`MS.occur` s) (nub w)

findValidWordsWRes :: Words -> Words -> Restrictions -> String
findValidWordsWRes ws _ res = recomendedGuesser possibleWords
  where
    conditions = toConditions res
    possibleWords = filterWords ws conditions

toConditions :: Restrictions -> [String -> Bool]
toConditions res = [c1,c2,c3,c4]
  where
    c1 = \x -> all (`elem` x) (nub $ M.foldr (++) "" $ includedNotAtPos res)
    c2 = \x -> all (`notElem` x) (notIncluded res)
    c3 = \x -> all (\(k,s) -> all (== (x !! k)) s) (M.toList $ includedAtPos res)
    c4 = \x -> all (\(k,s)-> (x !! k) `notElem` s) (M.toList $ includedNotAtPos res)

filterWords :: Words -> [String -> Bool] -> Words
filterWords ws conditions = filter (\w -> all (\c -> c w) conditions) ws
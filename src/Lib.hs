module Lib
    ( someFunc,
      findValidWords
    ) where

import Data.List.Split ( splitOn )
import System.Environment (getArgs)
import Data.List ( nub )
import qualified Data.MultiSet as MS

type CountSet = MS.MultiSet Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"


findValidWords :: IO ()
findValidWords = do
                    args <- getArgs
                    ws <- getWords
                    let conditions = toConditions $ parseInput $ args ++ ["","","",""]
                    let possibleWords = filterWords ws conditions
                    print possibleWords
                    print $ recomendedGuesser possibleWords


getWords :: IO [String]
getWords = do
    lines <$> readFile "words.txt"


filterWords :: [String] -> [String -> Bool] -> [String]
filterWords ws conditions = filter (\w -> all (\c -> c w) conditions) ws

parseInput :: [String] -> (String,String,[String],[String])
parseInput ws = (w0, w1, posIncluded, posNotIncluded)
                where
                    posIncluded = splitOn "," $ ws !! 2
                    posNotIncluded = splitOn "," $ ws !! 3
                    w0 | head (head ws) == '.' = ""
                       | otherwise = head ws
                    w1 | (ws !! 1) == "." = ""
                       | otherwise = ws !! 1

toConditions :: (String,String,[String],[String]) -> [String -> Bool]
toConditions (li,lni,pi,pni) = [c1,c2,c3,c4]
                               where
                                   c1 = \x -> all (`elem` x) li
                                   c2 = \x -> all (`notElem` x) lni
                                   c3 = \x -> all (\(a,b)-> all (== (x !! b)) a) (zip pi [0..])
                                   c4 = \x -> all (\(a,b)-> (x !! b) `notElem` a) (zip pni [0..])

recomendedGuesser :: [String] -> String
recomendedGuesser ws = optimalWord
                       where
                           counts = foldr (\w m -> foldr MS.insert m $ nub w) MS.empty ws
                           optimalWord = snd $ maximum $ zip (map (getWordValue counts) ws) ws
                           getWordValue :: CountSet -> String -> Int
                           getWordValue s w = sum $ map (`MS.occur` s) w'
                                            where
                                                w' = nub w

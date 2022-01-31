{-# LANGUAGE MultiWayIf #-}

module Game (startGame )
  where
import Control.Monad.State.Lazy
import qualified Data.Set as S
import System.Random
import Lib

type WordSet = S.Set String
data Game = Game
  {
    answer :: String,
    guess :: String,
    count :: Int,
    validGuesses :: WordSet
  }

initialiseGame :: String -> [String] -> Game
initialiseGame a wordList = Game a "" 0 (S.fromList $ map (take 5) wordList)

generateRandomWord :: IO String
generateRandomWord = do
  validS <- lines <$> readFile "valid_solutions.csv"
  seed <- newStdGen
  r <- getStdRandom (randomR (0,length validS - 1))
  return $ take 5 $ validS !! r

startGame :: IO ()
startGame = do
  validG <- lines <$> readFile "valid_guesses.csv"
  validS <- lines <$> readFile "valid_solutions.csv"
  word <- generateRandomWord
  game <- execStateT playGame (initialiseGame word $ validG ++ validS)
  putStrLn $ "You guessed the word " ++ answer game ++ " in " ++ show (count game) ++ " guesses!"

playGame :: StateT Game IO ()
playGame = do
  game <- get
  lift $ putStrLn "Enter a 5 letter word:"
  g <- lift getLine
  if | length g /= 5 -> do lift $ putStrLn "The word you entered was not 5 letters"
                           playGame
     | S.notMember g (validGuesses game) -> do lift $ putStrLn "Not a valid word"
                                               playGame
     | g == answer game -> do put $ game { count = 1 + count game }
                              lift $ putStrLn "OOOOO"
     | otherwise -> do put $ game { guess = g, count = 1 + count game }
                       resolveTurn
                       playGame

resolveTurn :: StateT Game IO ()
resolveTurn = do
  game <- get
  let g = guess game
  let a = answer game
  lift $ putStrLn $ showGuessResults a g

showGuessResults :: String -> String -> String
showGuessResults a g = map findFeedback g'
  where
    g' = zip g [0..]
    a' = zip a [0..]
    findFeedback :: (Char,Int) -> Char
    findFeedback (c,pos) | c == a !! pos = 'O'
                         | c `elem` a && all (\(c',pos') -> c' /= c || (c' == c && g !! pos' /= c')) a' = '|'
                         | otherwise = 'X'

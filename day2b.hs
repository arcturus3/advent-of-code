module Main (main) where

import Data.List
import Data.Maybe

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

data Outcome = Win | Draw | Loss

symbolToMove :: Char -> Move
symbolToMove 'A' = Rock
symbolToMove 'B' = Paper
symbolToMove 'C' = Scissors

symbolToOutcome :: Char -> Outcome
symbolToOutcome 'X' = Loss
symbolToOutcome 'Y' = Draw
symbolToOutcome 'Z' = Win

value :: Move -> Int
value Rock = 1
value Paper = 2
value Scissors = 3

score :: Move -> Move -> Int
score player opponent
  | player == opponent = 3
  | (player, opponent) `elem` wins = 6
  | otherwise = 0
  where
    wins = [(Rock, Scissors), (Scissors, Paper), (Paper, Rock)]

choose :: Move -> Outcome -> Move
choose move outcome =
  case outcome of
    Draw -> move
    Win -> fst $ fromJust $ find ((==move) . snd) fstBeatsSnd
    Loss -> snd $ fromJust $ find ((==move) . fst) fstBeatsSnd
  where
    fstBeatsSnd = [(Rock, Scissors), (Scissors, Paper), (Paper, Rock)]

solve :: [(Move, Outcome)] -> Int
solve =
  sum . map
    (\(opponent, outcome) ->
      let player = choose opponent outcome
      in value player + score player opponent
    )

parse :: String -> [(Move, Outcome)]
parse =
  map
    (\line ->
      let [x, y] = words line
      in
        (symbolToMove $ head x, symbolToOutcome $ head y)
    )
  . lines

main :: IO ()
main = do
  input <- getContents
  print $ solve $ parse input

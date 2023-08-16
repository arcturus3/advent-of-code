module Day2 (solve) where

import Data.List
import Data.Maybe

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

data Outcome = Win | Draw | Loss

symbolToMove :: Char -> Move
symbolToMove 'A' = Rock
symbolToMove 'B' = Paper
symbolToMove 'C' = Scissors
symbolToMove 'X' = Rock
symbolToMove 'Y' = Paper
symbolToMove 'Z' = Scissors

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

solve1 :: [(Move, Move)] -> Int
solve1 = sum . map (\(x, y) -> value y + score y x)

solve2 :: [(Move, Outcome)] -> Int
solve2 =
  sum . map
    (\(opponent, outcome) ->
      let player = choose opponent outcome
      in value player + score player opponent
    )

parse1 :: String -> [(Move, Move)]
parse1 =
  map
    (\line ->
      let [x, y] = words line
      in
        (symbolToMove $ head x, symbolToMove $ head y)
    )
  . lines

parse2 :: String -> [(Move, Outcome)]
parse2 =
  map
    (\line ->
      let [x, y] = words line
      in
        (symbolToMove $ head x, symbolToOutcome $ head y)
    )
  . lines

solve :: String -> IO ()
solve input = do
  print $ solve1 $ parse1 input
  print $ solve2 $ parse2 input

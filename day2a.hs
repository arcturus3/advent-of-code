module Main (main) where

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

symbolToMove :: Char -> Move
symbolToMove 'A' = Rock
symbolToMove 'B' = Paper
symbolToMove 'C' = Scissors
symbolToMove 'X' = Rock
symbolToMove 'Y' = Paper
symbolToMove 'Z' = Scissors

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

solve :: [(Move, Move)] -> Int
solve = sum . map (\(x, y) -> value y + score y x)

parse :: String -> [(Move, Move)]
parse =
  map
    (\line ->
      let [x, y] = words line
      in
        (symbolToMove $ head x, symbolToMove $ head y)
    )
  . lines

main :: IO ()
main = do
  input <- getContents
  print $ solve $ parse input

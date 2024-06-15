module Main (main) where

import System.Environment (getArgs)
import qualified Day1 (solve)
import qualified Day2 (solve)
import qualified Day3 (solve)
import qualified Day4 (solve)
import qualified Day5 (solve)
import qualified Day6 (solve)
import qualified Day7 (solve)
import qualified Day8 (solve)
import qualified Day9 (solve)
import qualified Day10 (solve)
import qualified Day11 (solve)
import qualified Day12 (solve)
import qualified Day13 (solve)
import qualified Day14 (solve)

solutions :: [String -> IO ()]
solutions =
  [ Day1.solve
  , Day2.solve
  , Day3.solve
  , Day4.solve
  , Day5.solve
  , Day6.solve
  , Day7.solve
  , Day8.solve
  , Day9.solve
  , Day10.solve
  , Day11.solve
  , Day12.solve
  , Day13.solve
  , Day14.solve
  ]

main :: IO ()
main = do
  day <- (read . (!! 0) <$> getArgs :: IO Int)
  input <- readFile ("input/day" ++ show day ++ ".txt")
  solutions !! (day - 1) $ input

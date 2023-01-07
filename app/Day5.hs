module Day5 (day5a, day5b) where

import Data.Char
import Data.List
import Data.List.Split

parseInput :: String -> ([[Char]], [[Int]])
parseInput = parseLines . break (== "") . lines
  where
    parseStacks = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseInstructions = map (map read . filter (all isDigit) . words) . tail
    parseLines (stacks, instructions) = (parseStacks stacks, parseInstructions instructions)

applyInstruction :: ([Char] -> [Char]) -> [[Char]] -> [Int] -> [[Char]]
applyInstruction order stacks [num, src, dst] =
  map (uncurry updateStack) (zip [1..] stacks)
  where
    movedCrates = take num $ stacks !! (src - 1)
    updateStack i stack
      | i == src = drop num stack
      | i == dst = order movedCrates ++ stack
      | otherwise = stack

day5a :: IO ()
day5a = do
  input <- parseInput <$> getContents
  let result = uncurry (foldl $ applyInstruction reverse) input
  print $ map head . filter (not . null) $ result

day5b :: IO ()
day5b = do
  input <- parseInput <$> getContents
  let result = uncurry (foldl $ applyInstruction id) input
  print $ map head . filter (not . null) $ result

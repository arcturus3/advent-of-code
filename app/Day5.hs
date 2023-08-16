module Day5 (solve) where

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

solve :: String -> IO ()
solve contents = do
  let input = parseInput contents
  let result1 = uncurry (foldl $ applyInstruction reverse) input
  let result2 = uncurry (foldl $ applyInstruction id) input
  print $ map head . filter (not . null) $ result1
  print $ map head . filter (not . null) $ result2

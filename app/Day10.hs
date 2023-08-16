module Day10 (solve) where

import System.IO (getContents')
import Data.List.Split (chunksOf)

deltas :: [String] -> [Int]
deltas =
  concatMap parse
  where
    parse op = if op == "noop"
      then [0]
      else [0, read $ words op !! 1]

solve :: String -> IO ()
solve input = do
  let ops = lines input
  let values = scanl (+) 1 (deltas ops)
  print $ sum $ map (\i -> i * values !! (i - 1)) [20, 60, 100, 140, 180, 220]

  let
    pixels = zipWith
      (\idx pos ->
        if abs ((idx `mod` 40) - pos) <= 1
          then '#'
          else '.'
      )
      [0..]
      values
  mapM_ putStrLn (chunksOf 40 pixels)

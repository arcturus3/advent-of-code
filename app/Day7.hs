module Day7 (solve) where

import Data.List (stripPrefix)
import Data.Char (isDigit)

sizes :: [Int] -> [String] -> [Int]
sizes stack [] = init $ scanl1 (+) stack
sizes stack@(p : ps) (cmd : cmds)
  | Just dir <- stripPrefix "$ cd " cmd = case dir of
    ".." -> p : sizes ((p + head ps) : tail ps) cmds
    _ -> sizes (0 : stack) cmds
  | size <- head $ words cmd, all isDigit size
    = sizes (((read size :: Int) + p) : ps) cmds
  | otherwise = sizes stack cmds

solve :: String -> IO ()
solve input = do
  let dirs = sizes [0] $ lines input
  print . sum . filter (<= 100000) $ dirs
  let required = maximum dirs - 40000000
  print . minimum . filter (>= required) $ dirs

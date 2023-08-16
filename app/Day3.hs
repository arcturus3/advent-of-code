module Day3 (solve) where

import Data.Char (ord)
import Data.List (intersect)

priority :: Char -> Int
priority item
  | ord 'a' <= ord item && ord item <= ord 'z' = ord item - ord 'a' + 1
  | otherwise = ord item - ord 'A' + 27

common1 :: String -> String -> Char
common1 s1 s2 =
  head $ intersect s1 s2

common2 :: (String, String, String) -> Char
common2 (s1, s2, s3) =
  head $ intersect s1 $ intersect s2 s3

sackPriority :: String -> Int
sackPriority s =
  let
    n = length s `div` 2
    (s1, s2) = splitAt n s
  in
    priority $ common1 s1 s2

solve1 :: [String] -> Int
solve1 =
  sum . map sackPriority

solve2 :: [(String, String, String)] -> Int
solve2 =
  sum . map (priority . common2)

groups :: [String] -> [(String, String, String)]
groups list =
  case list of
    [] -> []
    x:y:z:rest -> (x, y, z) : groups rest

solve :: IO ()
solve = do
  input <- getContents
  print $ solve1 $ lines input
  print $ solve2 $ groups $ lines input

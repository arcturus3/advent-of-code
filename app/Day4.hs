module Day4 (solve) where

import Data.List

split :: Char -> String -> (String, String)
split c s =
  let
    (s1, s2) = break (==c) s
  in
    (s1, drop 1 s2)

parse :: String -> ((Int, Int), (Int, Int))
parse s =
  let
    (r1, r2) = split ',' s
    (left1, right1) = split '-' r1
    (left2, right2) = split '-' r2
  in
    ((read left1 :: Int, read right1 :: Int)
    , (read left2 :: Int, read right2 :: Int)
    )

check1 :: ((Int, Int), (Int, Int)) -> Bool
check1 (r1, r2) =
  fst r1 <= fst r2 && snd r2 <= snd r1
  || fst r2 <= fst r1 && snd r1 <= snd r2

check2 :: ((Int, Int), (Int, Int)) -> Bool
check2 (r1, r2) =
  not (snd r1 < fst r2 || snd r2 < fst r1)

solve :: String -> IO ()
solve input = do
  print $ length $ filter (check1 . parse) $ lines input
  print $ length $ filter (check2 . parse) $ lines input

{-# LANGUAGE NumericUnderscores #-}

module Day15 where

import Data.ExtendedReal (Extended (Finite))
import Data.IntegerInterval (IntegerInterval, (<=..<=), singleton, memberCount, fromInterval, toInterval)
import Data.IntervalSet (IntervalSet, fromList, toList)
import Data.Maybe (fromJust)
import Text.Regex (mkRegex, matchRegex)

type Point = (Integer, Integer)

size :: IntervalSet Integer -> Integer
size = sum . map (fromJust . memberCount . fromInterval) . toList

coverage :: Integer -> (Point, Point) -> IntegerInterval
coverage row ((sx, sy), (bx, by)) =
  Finite left' <=..<= Finite right'
  where
    r = abs (bx - sx) + abs (by - sy)
    dy = abs (row - sy)
    left = sx - r + dy
    right = sx + r - dy
    left' = if by == row && bx == left then left + 1 else left
    right' = if by == row && bx == right then right - 1 else right

parseLine :: String -> (Point, Point)
parseLine line = ((sx, sy), (bx, by))
  where
    regex = mkRegex "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)"
    [sx, sy, bx, by] = map read $ fromJust $ matchRegex regex line

solve :: String -> IO ()
solve input = do
  print $ size . fromList $ map (toInterval . coverage 2_000_000 . parseLine) $ lines input

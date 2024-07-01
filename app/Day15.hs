{-# LANGUAGE NumericUnderscores #-}

module Day15 (solve) where

import Control.Applicative (liftA2)
import Data.List (find, findIndex)
import Data.ExtendedReal (Extended (Finite))
import Data.IntegerInterval (IntegerInterval, (<=..<=), fromInterval, toInterval, pickup, width)
import Data.IntervalSet as IntervalSet (IntervalSet, fromList, toList, difference, span, singleton, null)
import Text.Regex.TDFA ((=~))

type Point = (Integer, Integer)

size :: IntervalSet Integer -> Integer
size = sum . map ((+ 1) . width . fromInterval) . toList

beaconsImpossibleSensor :: Bool -> Integer -> (Point, Point) -> IntegerInterval
beaconsImpossibleSensor existingPossible row ((sx, sy), (bx, by)) =
  Finite left' <=..<= Finite right'
  where
    r = abs (bx - sx) + abs (by - sy)
    dy = abs (row - sy)
    left = sx - r + dy
    right = sx + r - dy
    left' = if by == row && bx == left && existingPossible then left + 1 else left
    right' = if by == row && bx == right && existingPossible then right - 1 else right

beaconsImpossibleSensors :: Bool -> Integer -> [(Point, Point)] -> IntervalSet Integer
beaconsImpossibleSensors existingPossible row =
  fromList . map (toInterval . beaconsImpossibleSensor existingPossible row)

beaconsPossibleSensors :: Integer -> Integer -> Bool -> Integer -> [(Point, Point)] -> IntervalSet Integer
beaconsPossibleSensors lowerBound upperBound existingPossible row =
  difference (singleton $ toInterval $ Finite lowerBound <=..<= Finite upperBound)
  . beaconsImpossibleSensors existingPossible row

parseLine :: String -> (Point, Point)
parseLine line = ((sx, sy), (bx, by))
  where
    regex = "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" 
    (_, _, _, matches) = line =~ regex :: (String, String, String, [String])
    [sx, sy, bx, by] = map read matches

solve :: String -> IO ()
solve input = do
  let info = map parseLine $ lines input
  print $ size $ beaconsImpossibleSensors True 2_000_000 info
  let (lowerBound, upperBound) = (0, 4_000_000)
  let allBeaconsPossible = map (flip (beaconsPossibleSensors lowerBound upperBound False) info) [lowerBound .. upperBound]
  let y = toInteger <$> findIndex (not . IntervalSet.null) allBeaconsPossible
  let x = pickup . fromInterval . IntervalSet.span =<< find (not . IntervalSet.null) allBeaconsPossible
  case liftA2 (,) x y of
    Nothing -> error "Invalid input"
    Just (x, y) -> print $ upperBound * x + y

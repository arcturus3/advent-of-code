module Day6 (day6a, day6b) where

import Data.List
import Data.Maybe

findMarker :: Int -> String -> Int
findMarker markerLength signal =
  fromJust (findIndex id markers) + markerLength
  where
    distinct list = length list == length (nub list)
    markers = map (\i -> distinct $ take markerLength $ drop i signal) [0..]

day6a :: String -> Int
day6a = findMarker 4

day6b :: String -> Int
day6b = findMarker 14

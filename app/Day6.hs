module Day6 (solve) where

import Data.List
import Data.Maybe

findMarker :: Int -> String -> Int
findMarker markerLength signal =
  fromJust (findIndex id markers) + markerLength
  where
    distinct list = length list == length (nub list)
    markers = map (\i -> distinct $ take markerLength $ drop i signal) [0..]

solve :: IO ()
solve = do
  input <- getContents
  print $ findMarker 4 input
  print $ findMarker 14 input

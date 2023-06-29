module Day8 where

import Data.List
import Data.Char (digitToInt)

visibleRow :: [Int] -> [Bool]
visibleRow heightmap =
  zipWith (>) heightmap ((-1) : runningMax)
  where runningMax = scanl1 max heightmap

visible :: [[Int]] -> [[Bool]]
visible heightmap =
  visibleLeft ||* visibleRight ||* visibleTop ||* visibleBottom
  where
    (||*) = zipWith (zipWith (||))
    reverse' = map reverse
    visibleLeft = map visibleRow heightmap
    visibleRight = reverse' . map visibleRow . reverse' $ heightmap
    visibleTop = transpose . map visibleRow . transpose $ heightmap
    visibleBottom = transpose . reverse' . map visibleRow . reverse' . transpose $ heightmap

main :: IO ()
main = do
  heightmap <- map (map digitToInt) . lines <$> getContents
  print $ sum . map (length . filter id) . visible $ heightmap

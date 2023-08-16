module Day8 (solve) where

import Data.List
import Data.Char (digitToInt)

-- visibility facing to the right
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

-- viewing distance facing to the right
distRow :: [Int] -> [Int]
distRow hs =
  map (\xs -> dist (head xs) (tail xs)) (init $ tails hs)
  where
    dist h hs =
      let (visible, invisible) = span (<h) hs
      in length visible + (if null invisible then 0 else 1)

dist :: [[Int]] -> [[Int]]
dist heightmap =
  distLeft ||* distRight ||* distTop ||* distBottom
  where
    (||*) = zipWith (zipWith (*))
    reverse' = map reverse
    distLeft = map distRow heightmap
    distRight = reverse' . map distRow . reverse' $ heightmap
    distTop = transpose . map distRow . transpose $ heightmap
    distBottom = transpose . reverse' . map distRow . reverse' . transpose $ heightmap

solve :: IO ()
solve = do
  heightmap <- map (map digitToInt) . lines <$> getContents
  print $ sum . map (length . filter id) . visible $ heightmap
  print $ maximum . map maximum . dist $ heightmap

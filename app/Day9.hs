module Day9 () where

import Data.Ord (clamp)
import Data.Char (digitToInt)
import Data.Set (fromList, size)

delta :: Char -> (Int, Int)
delta 'U' = (0, 1)
delta 'D' = (0, -1)
delta 'L' = (-1, 0)
delta 'R' = (1, 0)

-- old head offset from tail -> head movement -> new head offset from tail
transition :: (Int, Int) -> (Int, Int) -> (Int, Int)
transition (x, y) (dx, dy)
  | x' < -1 = (-1, 0)
  | x' > 1 = (1, 0)
  | y' < -1 = (0, -1)
  | y' > 1 = (0, 1)
  | otherwise = (x', y')
  where (x', y') = (x + dx, y + dy)

main :: IO ()
main = do
  let parseLine line = let items = words line in (items !! 0 !! 0, read (items !! 1) :: Int)
  input <- map parseLine . lines <$> getContents
  let deltas = map delta . concatMap (uncurry $ flip replicate) $ input
  let headPositions = scanl add (0, 0) deltas
  let offsets = scanl transition (0, 0) deltas
  let tailPositions = zipWith sub headPositions offsets
  print . size . fromList $ tailPositions
  where
    add (a, b) (c, d) = (a + c, b + d)
    sub (a, b) (c, d) = (a - c, b - d)

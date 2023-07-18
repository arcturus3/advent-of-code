module Day9 () where

import Data.Ord (clamp)
import Data.Char (digitToInt)
import Data.Set (fromList, size)

delta :: Char -> (Int, Int)
delta 'U' = (0, 1)
delta 'D' = (0, -1)
delta 'L' = (-1, 0)
delta 'R' = (1, 0)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty) =
  if (dx, dy) == (dx', dy')
    then (tx, ty)
    else (tx + dx', ty + dy')
  where
    (dx, dy) = (hx - tx, hy - ty)
    (dx', dy') = (clamp (-1, 1) dx, clamp (-1, 1) dy)

moveRope :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
moveRope rope (dx, dy) =
  scanl moveTail (hx', hy') (tail rope)
  where
    (hx, hy) = head rope
    (hx', hy') = (hx + dx, hy + dy)

solve :: [(Char, Int)] -> Int -> Int
solve moves length =
  size . fromList $ positions
  where
    deltas = map delta . concatMap (uncurry $ flip replicate) $ moves
    rope = replicate length (0, 0)
    ropes = scanl moveRope rope deltas
    positions = map last ropes

main :: IO ()
main = do
  let parseLine line = let [move, times] = words line in (head move, read times :: Int)
  input <- map parseLine . lines <$> getContents
  print $ solve input 2
  print $ solve input 10

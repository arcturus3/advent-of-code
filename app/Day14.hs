module Day14 where

import Control.Monad.State (State, state, evalState)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member, notMember, insert, union)


type Point = (Int, Int)

findRestPos :: Int -> Set Point -> Point -> Maybe Point
findRestPos bottomBound occ (x, y)
  | y > bottomBound = Nothing
  | member (x, y) occ = Nothing
  | notMember (x, y + 1) occ = findRestPos bottomBound occ (x, y + 1)
  | notMember (x - 1, y + 1) occ = findRestPos bottomBound occ (x - 1, y + 1)
  | notMember (x + 1, y + 1) occ = findRestPos bottomBound occ (x + 1, y + 1)
  | otherwise = Just (x, y)

fill :: State (Int, Set Point) Bool
fill = state $ \(bounds, occ) -> maybe
  (True, (bounds, occ))
  (\pos -> (False, (bounds, insert pos occ)))
  (findRestPos bounds occ (500, 0))

countFills :: Int -> Set Point -> Int
countFills = curry $ length . takeWhile not . evalState (sequence $ repeat fill)

solve :: String -> IO ()
solve input = do
  let
    paths = map
      (map
        ((\[x, y] -> (read x :: Int, read y :: Int))
          . splitOn ",")
        . splitOn " -> ")
      (lines input)
  let bottomBoundVoid = maximum $ map snd $ concat paths
  let bottomBoundFloor = bottomBoundVoid + 2
  let
    occVoid = fromList
      [ (x, y)
      | path <- paths
      , (p, q) <- zip path (tail path)
      , x <- [min (fst p) (fst q) .. max (fst p) (fst q)]
      , y <- [min (snd p) (snd q) .. max (snd p) (snd q)]
      ]
  let occFloor = occVoid `union` fromList [(500 + x, bottomBoundFloor) | x <- [-bottomBoundFloor .. bottomBoundFloor]]
  
  print $ countFills bottomBoundVoid occVoid
  print $ countFills bottomBoundFloor occFloor

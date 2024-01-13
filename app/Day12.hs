{-# LANGUAGE LambdaCase #-}

module Day12 (solve) where

import Data.List (find)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Char (ord)
import Data.Bifunctor (bimap)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

search :: [[Int]] -> Seq.Seq (Int, Int) -> Map.Map (Int, Int) Int -> (Seq.Seq (Int, Int), Map.Map (Int, Int) Int)
search grid Seq.Empty dists = (Seq.empty, dists)
search grid queue dists =
  search grid queue' dists'
  where
    (r, c) = fromJust $ Seq.lookup 0 queue
    dist = fromJust $ Map.lookup (r, c) dists
    inbounds (r, c) = 0 <= r && r < length grid && 0 <= c && c < length (head grid)
    reachable (r1, c1) (r2, c2) = grid !! r1 !! c1 + 1 >= grid !! r2 !! c2
    visited square = isJust $ Map.lookup square dists
    nbrs = map (bimap (+ r) (+ c)) [(0, 1), (0, -1), (1, 0), (-1, 0)]
    nbrs' = filter (and . (<*>) [inbounds, reachable (r, c), not . visited] . pure) nbrs
    queue' = Seq.drop 1 queue Seq.>< Seq.fromList nbrs'
    dists' = foldl (\dists nbr -> Map.insert nbr (dist + 1) dists) dists nbrs'

solve :: String -> IO ()
solve input = do
  let grid = lines input
  let grid' = map
        (map (\case
          'S' -> 0
          'E' -> ord 'z' - ord 'a'
          x -> ord x - ord 'a'
        ))
        grid
  let squares = [(r, c) | r <- [0 .. length grid - 1], c <- [0 .. length (head grid) - 1]]
  let src = fromJust $ find (\(r, c) -> grid !! r !! c == 'S') squares
  let dst = fromJust $ find (\(r, c) -> grid !! r !! c == 'E') squares
  let queue = Seq.singleton src
  let dists = Map.singleton src 0
  let (_, dists') = search grid' queue dists
  print $ fromJust $ Map.lookup dst dists'
  let srcs = filter (\(r, c) -> grid' !! r !! c == 0) squares
  let results = map (\src -> search grid' (Seq.singleton src) (Map.singleton src 0)) srcs
  print $ minimum $ mapMaybe (Map.lookup dst . snd) results

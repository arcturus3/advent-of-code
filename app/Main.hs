module Main where

import Day5 (day5a, day5b)
import Day6 (day6a, day6b)

main :: IO ()
main =
  getContents >>= print . day6b

module Main (main) where

import Data.List (sort)

main :: IO ()
main = do
    input <- getContents >>= (\contents -> putStrLn "" >> return contents)
    let calories = (map . map) (read :: String -> Int) (groupLines $ lines input)
    print $ solve2 calories

groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines lines =
    let (group, rest) = break (=="") lines
    in group : groupLines (drop 1 rest)

solve1 :: [[Int]] -> Int
solve1 calories =
    maximum $ map sum calories

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum

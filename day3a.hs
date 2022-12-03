import Data.Char (ord)
import Data.List (intersect)

priority :: Char -> Int
priority item
  | ord 'a' <= ord item && ord item <= ord 'z' = ord item - ord 'a' + 1
  | otherwise = ord item - ord 'A' + 27

common :: String -> String -> Char
common s1 s2 =
  head $ intersect s1 s2

sackPriority :: String -> Int
sackPriority s =
  let
    n = length s `div` 2
    (s1, s2) = splitAt n s
  in
    priority $ common s1 s2

solve :: [String] -> Int
solve =
  sum . map sackPriority

main :: IO ()
main = do
  input <- getContents
  print $ solve $ lines input

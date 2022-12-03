import Data.Char (ord)
import Data.List (intersect)

priority :: Char -> Int
priority item
  | ord 'a' <= ord item && ord item <= ord 'z' = ord item - ord 'a' + 1
  | otherwise = ord item - ord 'A' + 27

common :: (String, String, String) -> Char
common (s1, s2, s3) =
  head $ intersect s1 $ intersect s2 s3

solve :: [(String, String, String)] -> Int
solve =
  sum . map (priority . common)

groups :: [String] -> [(String, String, String)]
groups list =
  case list of
    [] -> []
    x:y:z:rest -> (x, y, z) : groups rest

main :: IO ()
main = do
  input <- getContents
  print $ solve $ groups $ lines input

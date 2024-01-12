module Day11 (solve) where

import Data.Void (Void)
import Text.Megaparsec (parse, sepBy, (<|>), Parsec, many)
import Text.Megaparsec.Char (char, string, space, newline)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)
import Data.List (sort, sortBy)
import qualified Data.Ord
import Data.Ord (comparing)

data Monkey = Monkey
  { items :: [Integer]
  , op :: Integer -> Integer
  , test :: Integer -> Int
  , count :: Integer
  , testNum :: Integer
  }

instance Show Monkey where
  show monkey = show (items monkey) ++ " " ++ show (count monkey)

type Parser = Parsec Void String

parseId :: Parser Int
parseId = do
  string "Monkey "
  id <- L.decimal
  char ':'
  pure id

parseItems :: Parser [Integer]
parseItems = do
  string "Starting items: "
  L.decimal `sepBy` string ", "

parseArg :: Parser (Maybe Integer)
parseArg = (Nothing <$ string "old") <|> (Just <$> L.decimal)

parseOp :: Parser (Integer -> Integer)
parseOp = do
  string "Operation: new = "
  left <- parseArg
  space
  op <- ((+) <$ char '+') <|> ((*) <$ char '*')
  space
  right <- parseArg
  pure (\old -> fromMaybe old left `op` fromMaybe old right)

parseTest :: Parser (Integer -> Int, Integer)
parseTest = do
  string "Test: divisible by "
  num <- L.decimal
  space
  true <- string "If true: throw to monkey " >> L.decimal
  space
  false <- string "If false: throw to monkey " >> L.decimal
  pure (\level -> if level `mod` num == 0 then true else false, num)

parseMonkey :: Parser Monkey
parseMonkey = do
  id <- parseId
  space
  items <- parseItems
  space
  op <- parseOp
  space
  (test, testNum) <- parseTest
  space
  pure Monkey
    { items = items
    , op = op
    , test = test
    , count = 0
    , testNum = testNum
    }

parseMonkeys :: Parser [Monkey]
parseMonkeys = many parseMonkey

doThrow :: Bool -> Int -> [Monkey] -> [Monkey]
doThrow divideWorry i monkeys =
  monkeys''
  where
    src = monkeys !! i
    src' = src { items = tail $ items src, count = count src + 1 }
    item = head $ items src
    item' = if divideWorry
      then op src item `div` 3
      else op src item `mod` product (map testNum monkeys)
    j = test src item'
    dst = monkeys !! j
    dst' = dst { items = items dst ++ [item'] }
    (before, _:after) = splitAt i monkeys
    monkeys' = before ++ [src'] ++ after
    (before', _:after') = splitAt j monkeys'
    monkeys'' = before' ++ [dst'] ++ after'

doThrows :: Bool -> Int -> [Monkey] -> [Monkey]
doThrows divideWorry i = until (null . items . (!! i)) (doThrow divideWorry i)

doRound :: Bool -> [Monkey] -> [Monkey]
doRound divideWorry monkeys = foldl (flip ($)) monkeys (map (doThrows divideWorry) [0..length monkeys - 1])

doRounds :: Bool -> Int -> [Monkey] -> [Monkey]
doRounds divideWorry rounds monkeys = iterate (doRound divideWorry) monkeys !! rounds

getMonkeyBusiness :: Bool -> Int -> [Monkey] -> Integer
getMonkeyBusiness divideWorry rounds monkeys =
  product $ take 2 $ sortBy (comparing Data.Ord.Down) $ map count $ doRounds divideWorry rounds monkeys

solve :: String -> IO ()
solve input = do
  let monkeys = parse parseMonkeys "" input
  case monkeys of
    Left _ ->
      print "parse error"
    Right monkeys -> do
      print $ getMonkeyBusiness True 20 monkeys
      print $ getMonkeyBusiness False 10000 monkeys

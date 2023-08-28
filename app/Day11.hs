module Day11 (solve) where

import Data.Void (Void)
import Text.Megaparsec (parse, sepBy, (<|>), Parsec, many)
import Text.Megaparsec.Char (char, string, space, newline)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)

data Monkey = Monkey
  { items :: [Int]
  , op :: Int -> Int
  , test :: Int -> Int
  }

instance Show Monkey where
  show = show . items

type Parser = Parsec Void String

parseId :: Parser Int
parseId = do
  string "Monkey "
  id <- L.decimal
  char ':'
  pure id

parseItems :: Parser [Int]
parseItems = do
  string "Starting items: "
  L.decimal `sepBy` string ", "

parseArg :: Parser (Maybe Int)
parseArg = (Nothing <$ string "old") <|> (Just <$> L.decimal)

parseOp :: Parser (Int -> Int)
parseOp = do
  string "Operation: new = "
  left <- parseArg
  space
  op <- ((+) <$ char '+') <|> ((*) <$ char '*')
  space
  right <- parseArg
  pure (\old -> fromMaybe old left `op` fromMaybe old right)

parseTest :: Parser (Int -> Int)
parseTest = do
  string "Test: divisible by "
  num <- L.decimal
  space
  true <- string "If true: throw to monkey " >> L.decimal
  space
  false <- string "If false: throw to monkey " >> L.decimal
  pure (\level -> if level `mod` num == 0 then true else false)

parseMonkey :: Parser Monkey
parseMonkey = do
  id <- parseId
  space
  items <- parseItems
  space
  op <- parseOp
  space
  test <- parseTest
  space
  pure Monkey
    { items = items
    , op = op
    , test = test
    }

parseMonkeys :: Parser [Monkey]
parseMonkeys = many parseMonkey

solve :: String -> IO ()
solve input = do
  print $ parse parseMonkeys "" input

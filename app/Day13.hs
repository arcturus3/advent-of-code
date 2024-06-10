{-# LANGUAGE InstanceSigs #-}

module Day13 where

import Control.Applicative (liftA2)
import Data.Char (isDigit, digitToInt)
import Data.List (sort, elemIndex)
import Data.List.Split (chunksOf)
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), sepBy, parse, errorBundlePretty)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Packet = Integer Int | List [Packet] deriving Eq

packetParser :: Parser Packet
packetParser = (Integer <$> decimal) <|> (char '[' *> (List <$> (packetParser `sepBy` char ',')) <* char ']')

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Integer left) (Integer right) = compare left right
  compare (Integer left) (List right) = compare (List [Integer left]) (List right)
  compare (List left) (Integer right) = compare (List left) (List [Integer right])
  compare (List left) (List right) = compare left right

solve :: String -> IO ()
solve input = do
  let parses = map (parse packetParser "") $ filter (/= "") $ lines input
  case sequence parses of
    Left e -> error $ errorBundlePretty e
    Right packets -> do
      let pairs = chunksOf 2 packets
      print $ sum [i | (i, [p, q]) <- zip [1..] pairs, p <= q]
      let (divider1, divider2) = (List [List [Integer 2]], List [List [Integer 6]])
      let allPackets = sort $ divider1 : divider2 : packets
      let key = liftA2 (*) ((1 +) <$> elemIndex divider1 allPackets) ((1 +) <$> elemIndex divider2 allPackets)
      case key of
        Nothing -> error "Missing divider packet(s)"
        Just value -> print value

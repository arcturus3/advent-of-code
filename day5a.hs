import Data.List (break)

type Stack = [Char]
type Instruction = (Int, Int, Int)

getCrates :: String -> [Maybe Char]
getCrates [] = []
getCrates input =
  (if input !! 1 == ' '
    then Nothing
    else Just (input !! 1)
  ) : getCrates (drop 4 input)

buildStacks :: Int -> [[Maybe Char]] -> [Stack]
buildStacks stackCount [] = replicate stackCount []
buildStacks stackCount (crates:rest) =
  zipWith addCrate oldStacks crates
  where
    oldStacks = buildStacks stackCount rest
    addCrate :: Stack -> Maybe Char -> Stack
    addCrate stack crate =
      case crate of
        Nothing -> stack
        Just x -> x : stack

applyAtIndex :: (a -> a) -> Int -> [a] -> [a]
applyAtIndex f i l =
  let
    applyAtIndex' :: (a -> a) -> Int -> [(Int, a)] -> [(Int, a)]
    applyAtIndex' f i [] = []
    applyAtIndex' f i (item:rest) =
      (if i == fst item
        then (fst item, f (snd item))
        else item
      ) : applyAtIndex' f i rest
  in
    map snd $ applyAtIndex' f i (zip [0..] l)

applyInstruction :: Instruction -> [Stack] -> [Stack]
applyInstruction (num, from, to) stacks
  | num == 1 = newStacks
  | otherwise = applyInstruction (num - 1, from, to) newStacks
  where
    newStacks =
      applyAtIndex (head (stacks !! (from - 1)) :) (to - 1)
      $ applyAtIndex (drop 1) (from - 1)
      stacks

parseInstruction :: String -> Instruction
parseInstruction input =
  let
    parts = words input
  in
    (read $ parts !! 1 :: Int, read $ parts !! 3 :: Int, read $ parts !! 5 :: Int)

parse :: String -> ([Stack], [Instruction])
parse input =
  let
    (stackSection', instructionSection') = break (=="\n") $ lines input
    stackCount = read $ last $ words $ last stackSection' :: Int
    stackSection = drop 1 $ reverse stackSection'
    instructionSection = drop 1 instructionSection'
    stacks = buildStacks stackCount (map getCrates stackSection)
    instructions = map parseInstruction instructionSection
  in
    (stacks, instructions)


solve :: [Stack] -> [Instruction] -> [Stack]
solve stacks [] = stacks
solve stacks (instruction:rest) =
  solve (applyInstruction instruction stacks) rest

main :: IO ()
main = do
  input <- getContents
  let (stacks, instructions) = parse input
  print stacks
  print instructions
  print $ solve stacks instructions

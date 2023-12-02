{-# LANGUAGE TypeFamilies #-}
module Day1.Solution (Day1(..)) where

import Parts

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (findIndex)

data Day1 = Day1 deriving Show

instance InputName Day1 where
  exampleName = const "Day1/example.txt"
  inputName = const "Day1/input.txt"

instance Part1 Day1 where
  type Input Day1 = [String]
  parse1 _ = map (filter isDigit) . lines
  solve1 _ = Result . sum . map extractNumber
    where
      extractNumber :: String -> Int
      extractNumber []     = undefined
      extractNumber xs = read [head xs, last xs]

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] [] = True
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

instance Part2 Day1 where
  parse2 d = parse1 d . unlines . map go . lines
    where
      names = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      go [] = []
      go (d:xs) | isDigit d = d : go xs
      go input =
        case findIndex (`startsWith` input) names of
          Just i -> head (show i) : go (tail input)
          Nothing -> go (tail input)

  solve2 = solve1

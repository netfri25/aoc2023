{-# OPTIONS_GHC -Wall -Wextra #-}
module Day1.Solution (Day1(..)) where

import Parts

import Data.Char (isDigit)
import Data.List (findIndex)

data Day1 = Day1 deriving Show

instance InputPath Day1 where
  examplePath = const "Day1/example.txt"
  inputPath = const "Day1/input.txt"

instance Part1 Day1 [String] where
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

instance Part2 Day1 [String] where
  parse2 day = parse1 day . unlines . map go . lines
    where
      names = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      go [] = []
      go (digit:xs) | isDigit digit = digit : go xs
      go input =
        case findIndex (`startsWith` input) names of
          Just i -> head (show i) : go (tail input)
          Nothing -> go (tail input)

  solve2 = solve1

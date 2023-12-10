{-# OPTIONS_GHC -Wall -Wextra #-}
module Day1.Solution (Day1(..)) where

import Parts

import Data.Char (isDigit)
import Data.List (findIndex, isPrefixOf)

data Day1 = Day1 deriving Show

instance Part1 Day1 [String] where
  parse1 _ = map (filter isDigit) . lines
  solve1 _ = show . sum . map extractNumber
    where
      extractNumber :: String -> Int
      extractNumber [] = 0
      extractNumber xs = read [head xs, last xs]

instance Part2 Day1 [String] where
  parse2 day = parse1 day . unlines . map go . lines
    where
      names = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      go [] = []
      go (digit:xs) | isDigit digit = digit : go xs
      go input =
        case findIndex (`isPrefixOf` input) names of
          Just i -> head (show i) : go (tail input)
          Nothing -> go (tail input)

  solve2 = solve1

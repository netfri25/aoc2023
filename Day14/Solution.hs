{-# OPTIONS_GHC -Wall -Wextra #-}
module Day14.Solution (Day14(..)) where

import Parts

import Data.List (transpose)

data Day14 = Day14 deriving Show

data Rock = Roll | Static
  deriving (Show, Eq)

instance {-# OVERLAPS #-} Show (Maybe Rock) where
  show Nothing = "."
  show (Just Roll) = "O"
  show (Just Static) = "#"

type Grid = [[Maybe Rock]]

instance Part1 Day14 Grid where
  parse1 _ = map (map $ flip lookup [('O', Roll), ('#', Static)]) . lines
  solve1 _ = show . calcLoad

calcLoad :: Grid -> Int
calcLoad grid = sum $ map (countLine max_y) $ transpose grid
  where max_y = length $ head grid

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

countLine :: Int -> [Maybe Rock] -> Int
countLine _ [] = 0
countLine high line = load + countLine (high - length not_static - 1) (safeTail rest)
  where
    load = sum $ take (length rolls) mults
    mults = iterate pred high
    (not_static, rest) = break (==Just Static) line
    rolls = filter (==Just Roll) not_static

instance Part2 Day14 Grid where
  parse2 = parse1
  solve2 _ = show . const ()

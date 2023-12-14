{-# OPTIONS_GHC -Wall -Wextra #-}
module Day14.Solution (Day14(..)) where

import Parts

import Data.Array

data Day14 = Day14 deriving Show

data Rock = Roll | Static
  deriving (Show, Eq)

type Grid = Array (Int, Int) (Maybe Rock)

instance Part1 Day14 Grid where
  parse1 _ input = array ((1, 1), (length $ head $ lines input, length $ lines input))
    [ ((x, y), rock)
    | (y, line) <- zip [1..] $ lines input
    , (x, c) <- zip [1..] line
    , let rock = lookup c [('O', Roll), ('#', Static)]
    ]
  solve1 _ grid = show $ sum $ map (countLine max_y . column) [1 .. fst (snd $ bounds grid)]
    where
      max_y = snd $ snd $ bounds grid
      column x = map (\y -> grid ! (x, y)) [1..max_y]

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

countLine :: Int -> [Maybe Rock] -> Int
countLine _ [] = 0
countLine high line = load + countLine (high - length not_static - 1) (safeTail rest)
  where
    load = sum $ take (length $ filter (==Just Roll) not_static) mults
    mults = iterate pred high
    (not_static, rest) = break (==Just Static) line

instance Part2 Day14 () where
  parse2 _ = const ()
  solve2 _ = show . const ()

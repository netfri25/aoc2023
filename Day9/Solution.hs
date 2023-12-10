{-# OPTIONS_GHC -Wall -Wextra #-}
module Day9.Solution (Day9(..)) where

import Parts

data Day9 = Day9 deriving Show

instance Part1 Day9 [[Int]] where
  parse1 _ = map (map read . words) . lines
  solve1 _ = show . sum . map predict

predict :: [Int] -> Int
predict xs
  | all (==0) xs = 0
  | otherwise = last xs + predict diff
  where diff = zipWith (-) (tail xs) (init xs)

instance Part2 Day9 [[Int]] where
  parse2 = parse1
  solve2 day = solve1 day . map reverse

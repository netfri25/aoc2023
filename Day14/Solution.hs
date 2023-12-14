{-# OPTIONS_GHC -Wall -Wextra #-}
module Day14.Solution (Day14(..)) where

import Parts

import Data.List (transpose, partition, uncons, elemIndex)
import Data.Bifunctor (second)
import Data.Maybe (fromJust)

data Day14 = Day14 deriving Show

data Rock = Roll | Static
  deriving Eq

instance {-# OVERLAPS #-} Show (Maybe Rock) where
  show Nothing = "."
  show (Just Roll) = "O"
  show (Just Static) = "#"

type Grid = [[Maybe Rock]]

instance Part1 Day14 Grid where
  parse1 _ = map (map $ flip lookup [('O', Roll), ('#', Static)]) . lines
  solve1 _ = show . calcLoad . transpose

calcLoad :: Grid -> Int
calcLoad grid = sum $ map (countLine row_length) grid
  where row_length = length $ head grid

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

simpleLoadLineRev :: [Maybe Rock] -> Int
simpleLoadLineRev = sum . map fst . filter ((==Just Roll) . snd) . zip [1..]

simpleLoad :: Grid -> Int
simpleLoad = sum . map (simpleLoadLineRev . reverse)

instance Part2 Day14 Grid where
  parse2 = parse1
  solve2 _ grid = show $ simpleLoad $ transpose result_mirror
    where
      total_rotations = 1000000000
      previous_mirrors = reverse $ findCycle grid
      mirror = last previous_mirrors
      idx_of_first_mirror = fromJust $ elemIndex mirror previous_mirrors
      cycle_length = length previous_mirrors - idx_of_first_mirror - 1
      iterations = (total_rotations - idx_of_first_mirror) `rem` cycle_length
      result_mirror = iterate fullCycle mirror !! iterations

fullCycle :: Grid -> Grid
fullCycle = slideEast . slideSouth . slideWest . slideNorth

findCycle :: Grid -> [Grid]
findCycle = go []
  where
    go :: [Grid] -> Grid -> [Grid]
    go history grid
      | grid `elem` history = grid : history
      | otherwise = go (grid : history) (fullCycle grid)

reconstructLine :: [Maybe Rock] -> [Maybe Rock]
reconstructLine [] = []
reconstructLine line = rollable_rocks ++ no_rocks ++ maybe [] (uncurry (:) . second reconstructLine) (uncons rest)
  where
    (not_static, rest) = break (==Just Static) line
    (no_rocks, rollable_rocks) = partition (==Nothing) not_static

slideNorth :: Grid -> Grid
slideNorth = transpose . map reconstructLine . transpose

slideWest :: Grid -> Grid
slideWest = map reconstructLine

slideSouth :: Grid -> Grid
slideSouth = transpose . map (reverse . reconstructLine . reverse) . transpose

slideEast :: Grid -> Grid
slideEast = map (reverse . reconstructLine . reverse)

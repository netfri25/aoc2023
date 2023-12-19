{-# OPTIONS_GHC -Wall -Wextra #-}
module Day11.Solution (Day11(..)) where

import Parts

import Data.List (tails, transpose)

data Day11 = Day11 deriving Show

data Point = Point Int Int
  deriving (Eq, Ord)

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

parseGalaxies :: (Point -> Point) -> [String] -> [Point]
parseGalaxies expand input =
  [ expand $ Point x y
  | (y, line) <- zip [0..] input
  , (x, c) <- zip [0..] line
  , c == '#'
  ]

expansion :: [Bool] -> [Int]
expansion = scanl1 (+) . map fromEnum

expandPoint :: [Int] -> [Int] -> Point -> Point
expandPoint expansion_xs expansion_ys (Point x y) = Point (x + expansion_xs !! x) (y + expansion_ys !! y)

inputExpansions :: Int -> String -> ([Int], [Int])
inputExpansions mult input = (map (*mult') expansion_cols, map (*mult') expansion_rows)
  where
    mult' = pred mult
    rows = lines input
    cols = transpose rows
    should_expaned_rows = map (all (=='.')) rows
    should_expaned_cols = map (all (=='.')) cols
    expansion_rows = expansion should_expaned_rows
    expansion_cols = expansion should_expaned_cols

instance Part1 Day11 [Point] where
  parse1 _ input = parseGalaxies (uncurry expandPoint $ inputExpansions 2 input) $ lines input
  solve1 _ galaxies = show $ sum [distance g1 g2 | (g1:gs) <- init $ tails galaxies, g2 <- gs]

instance Part2 Day11 [Point] where
  parse2 _ input = parseGalaxies (uncurry expandPoint $ inputExpansions 1000000 input) $ lines input
  solve2 = solve1

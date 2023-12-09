{-# OPTIONS_GHC -Wall -Wextra #-}
module Day9.Solution (Day9(..)) where

import Parts
import Parser

import Data.Maybe (fromJust)
import Control.Monad (mfilter)

data Day9 = Day9 deriving Show

parseNumList :: Parser String [Int]
parseNumList = mfilter (not . null) $ sepBy (eqP ' ') numP

instance Part1 Day9 [[Int]] where
  parse1 _ = fst . fromJust . runParserT (sepBy (eqP '\n') parseNumList)
  solve1 _ = Result . sum . map predict

predict :: [Int] -> Int
predict xs
  | all (==0) xs = 0
  | otherwise = last xs + predict diff
  where diff = zipWith (-) (tail xs) (init xs)

instance Part2 Day9 [[Int]] where
  parse2 = parse1
  solve2 day = solve1 day . map reverse

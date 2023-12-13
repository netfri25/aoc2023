{-# OPTIONS_GHC -Wall -Wextra #-}
module Day13.Solution (Day13(..)) where

import Parts
import Parser

import Control.Applicative (Alternative (..))
import Control.Monad (mfilter)
import Data.List (inits, tails, transpose)
import Data.Maybe (listToMaybe)
import Data.Foldable (fold)

data Day13 = Day13 deriving Show

type Pattern = [[Int]]

parsePattern :: Input i Char => Parser i Pattern
parsePattern = mfilter (not . null) $ sepBy (eqP '\n') (some $ (1 <$ eqP '#') <|> (0 <$ eqP '.'))

instance Part1 Day13 [Pattern] where
  parse1 _ = runParser $ sepBy ws parsePattern
  solve1 _ = show . sum . fold . traverse (patternValue 0)

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

findMirror :: Similar a => Int -> [a] -> Maybe Int
findMirror non_similarity xs = listToMaybe [length lhs | (lhs, rhs) <- init $ tail $ splits xs, nonSimilar (reverse lhs) rhs == non_similarity]

patternValue :: Int -> Pattern -> Maybe Int
patternValue non_similarity pat = (*100) <$> findMirror non_similarity pat <|> findMirror non_similarity (transpose pat)

instance Part2 Day13 [Pattern] where
  parse2 = parse1
  solve2 _ = show . sum . fold . traverse (patternValue 1)

class Similar a where
  similar :: a -> a -> Int
  nonSimilar :: a -> a -> Int

instance Similar Int where
  similar x y = fromEnum $ x == y
  nonSimilar x y = 1 - similar x y

instance Similar a => Similar [a] where
  similar xs ys = sum $ zipWith similar xs ys
  nonSimilar xs ys = sum $ zipWith nonSimilar xs ys

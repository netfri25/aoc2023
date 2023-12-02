{-# LANGUAGE TypeFamilies #-}

module Parts
  ( Result(..)
  , Todo(..)
  , Part1(..)
  , part1
  , Part2(..)
  , part2
  , InputPath(..)
  ) where

-- the type I wrap every day result with
-- requires the type to have an instance of the Show class
data Result where
  Result :: Show a => a -> Result

data Todo = Todo deriving Show

instance Show Result where
  show (Result x) = show x


class InputPath a where
  examplePath :: a -> FilePath
  inputPath :: a -> FilePath


class Part1 a where
  type Input a
  parse1 :: a -> String -> Input a
  solve1 :: a -> Input a -> Result

part1 :: Part1 a => a -> String -> Result
part1 day = solve1 day . parse1 day


class (Part1 a) => Part2 a where
  parse2 :: a -> String -> Input a
  solve2 :: a -> Input a -> Result

part2 :: Part2 a => a -> String -> Result
part2 day = solve2 day . parse2 day

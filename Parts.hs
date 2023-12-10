{-# LANGUAGE FunctionalDependencies #-}

module Parts
  ( Result(..)
  , Todo(..)
  , Part1(..)
  , part1
  , Part2(..)
  , part2
  ) where

import Control.DeepSeq (NFData(..), force)

-- the type I wrap every day result with
-- requires the type to have an instance of the Show class
data Result where
  Result :: (Show a, NFData a) => a -> Result

instance NFData Result where
  rnf (Result x) = rnf x

data Todo = Todo deriving Show
instance NFData Todo where
  rnf _ = ()

instance Show Result where
  show (Result x) = show x


class Part1 day input | day -> input where
  parse1 :: day -> String -> input
  solve1 :: day -> input -> Result

part1 :: Part1 day input => day -> String -> Result
part1 day = solve1 day . parse1 day


class Part2 day input | day -> input where
  parse2 :: day -> String -> input
  solve2 :: day -> input -> Result

part2 :: Part2 day input => day -> String -> Result
part2 day = solve2 day . parse2 day

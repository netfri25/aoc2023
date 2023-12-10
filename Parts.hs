{-# LANGUAGE FunctionalDependencies #-}

module Parts
  ( Part1(..)
  , part1
  , Part2(..)
  , part2
  ) where

import Control.DeepSeq (NFData(..), force)

class Part1 day input | day -> input where
  parse1 :: day -> String -> input
  solve1 :: day -> input -> String

part1 :: Part1 day input => day -> String -> String
part1 day = solve1 day . parse1 day


class Part2 day input | day -> input where
  parse2 :: day -> String -> input
  solve2 :: day -> input -> String

part2 :: Part2 day input => day -> String -> String
part2 day = solve2 day . parse2 day

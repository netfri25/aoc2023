{-# OPTIONS_GHC -Wall -Wextra #-}
module Day12.Solution (Day12(..)) where

import Parts
import Parser
import Control.Applicative (Alternative (some))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

data Day12 = Day12 deriving Show

data Spring = Good | Bad
  deriving (Eq)

instance Show Spring where
  show Good = "."
  show Bad  = "#"

instance {-# OVERLAPS #-} Show (Maybe Spring) where
  show Nothing = "?"
  show (Just x) = show x

parseSpring :: Input i Char => Parser i (Maybe Spring)
parseSpring = nextP >>= lift . flip lookup [('?', Nothing), ('.', Just Good), ('#', Just Bad)]

type Group = Int

parseGroups :: Input i Char => Parser i [Group]
parseGroups = sepBy (eqP ',') numP

instance Part1 Day12 [([Maybe Spring], [Group])] where
  parse1 _ = runParser $ sepBy ws ((,) <$> some parseSpring <* ws <*> parseGroups)
  solve1 _ = show . sum . map (uncurry springsCombs)

springsCombs :: [Maybe Spring] -> [Group] -> Int
springsCombs springs [] = fromEnum $ Just Bad `notElem` springs
springsCombs [] _ = 0
springsCombs (Nothing : springs) gs = springsCombs (Just Good : springs) gs + springsCombs (Just Bad : springs) gs
springsCombs (Just Good : springs) gs = springsCombs springs gs
springsCombs (Just Bad : springs) (g:gs) = fromMaybe 0 $ do
  let xs = take g (springs ++ repeat (Just Good))
  let good = last xs
  guard $ Just Good `notElem` init xs
  guard $ good /= Just Bad
  return $ springsCombs (drop g springs) gs

instance Part2 Day12 () where
  parse2 _ = const ()
  solve2 _ = show . const ()

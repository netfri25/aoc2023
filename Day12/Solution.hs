{-# OPTIONS_GHC -Wall -Wextra #-}
module Day12.Solution (Day12(..)) where

import Parts
import Parser
import Control.Applicative (Alternative (some))
import Control.Monad (guard, unless)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.State
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.List (intercalate)

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

type Memo k v = State (M.Map k v)

springsCombs :: [Maybe Spring] -> [Group] -> Int
springsCombs springs' gs' = flip evalState mempty $ springsCombs' (zip [1..] springs') (zip [1..] gs')
  where
    springsCombs' :: [(Int, Maybe Spring)] -> [(Int, Group)] -> Memo (Int, Int) Int Int
    springsCombs' springs [] = return $ fromEnum $ Just Bad `notElem` map snd springs
    springsCombs' [] _ = return 0
    springsCombs' ((_i, Just Good) : springs) gs = springsCombs' springs gs
    springsCombs' ((_i, Just Bad) : springs) ((_j, g):gs) = fromMaybe (return 0) $ do
      let xs = take g (springs ++ repeat (undefined, Just Good))
      let (_, good) = last xs
      guard $ Just Good `notElem` map snd (init xs)
      guard $ good /= Just Bad
      return $ springsCombs' (drop g springs) gs
    springsCombs' ((i, Nothing) : springs) gs = do
      let j = maybe (-1) fst $ listToMaybe gs
      has <- gets (M.member (i, j))
      unless has (evaluate j)
      gets (M.! (i, j))
      where
        evaluate :: Int -> Memo (Int, Int) Int ()
        evaluate j = do
          lhs <- springsCombs' ((i, Just Good) : springs) gs
          rhs <- springsCombs' ((i, Just Bad) : springs) gs
          let result = lhs + rhs
          modify (M.insert (i, j) result)


instance Part2 Day12 [([Maybe Spring], [Group])] where
  parse2 day = map (bimap update_springs update_counts) . parse1 day
    where
      update_springs = intercalate [Nothing] . replicate 5
      update_counts = concat . replicate 5
  solve2 = solve1

{-# OPTIONS_GHC -Wall -Wextra #-}
module Day21.Solution (Day21(..)) where

import Parts

import Data.Array
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad (forM)

data Day21 = Day21 deriving Show

data Cell = Empty | Start | Wall
  deriving (Show, Eq)

data Point = Point Int Int
  deriving (Show, Eq, Ord, Ix)

up, down, left, right :: Point
up = Point 0 (-1)
down = Point 0 1
left = Point (-1) 0
right = Point 1 0

instance Num Point where
  Point x1 y1 + Point x2 y2 = Point (x1+x2) (y1+y2)
  Point x1 y1 * Point x2 y2 = Point (x1*x2) (y1*y2)
  abs (Point x y) = Point (abs x) (abs y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger n = Point (fromInteger n) (fromInteger n)
  negate (Point x y) = Point (negate x) (negate y)

type Grid = Array Point Cell

parseGrid :: String -> Grid
parseGrid input = array (Point 0 0, Point (pred cols) (pred rows)) $ do
  (y, line) <- zip [0..] $ lines input
  (x, c) <- zip [0..] line
  let cell = fromJust $ lookup c [('.', Empty), ('S', Start), ('#', Wall)]
  return (Point x y, cell)
  where
    rows = length $ lines input
    cols = length $ head $ lines input

type Counter = M.Map Point Int

around :: Point -> [Point]
around p = map (p+) [up, down, left, right]

stepGrid :: Int -> Point -> (Point -> Bool) -> Counter
stepGrid max_steps start can_step = flip execState mempty $ stepGrid' 0 [start]
  where
    stepGrid' :: Int -> [Point] -> State Counter [Point]
    stepGrid' step points
      | step > max_steps = return []
      | otherwise = do
        new_points <- forM points $ \point -> do
          counter <- get
          if M.member point counter
          then return []
          else do
            modify $ M.insert point step
            return $ filter can_step $ around point
        stepGrid' (succ step) (concat new_points)

instance Part1 Day21 Grid where
  parse1 _ = parseGrid
  solve1 _ grid = show $ M.size $ M.filter even $ stepGrid 64 start can_step
    where
      start = fromJust $ lookup Start $ map (uncurry $ flip (,)) $ assocs grid
      can_step p = inRange (bounds grid) p && grid ! p == Empty

instance Part2 Day21 Grid where
  parse2 = parse1
  solve2 _ = show . const ()

{-# OPTIONS_GHC -Wall -Wextra #-}
module Day17.Solution (Day17(..)) where

import Parts

import Data.Array
import Data.Char (digitToInt)
import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad (when, unless, forM_)
import Data.Maybe (fromJust)

data Day17 = Day17 deriving Show

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

type Grid = Array Point Int

instance Part1 Day17 Grid where
  parse1 _ input = array (Point 1 1, Point cols rows)
    [ (Point x y, num)
    | (y, line) <- zip [1..] $ lines input
    , (x, letter) <- zip [1..] line
    , let num = digitToInt letter
    ]
    where
      rows = length $ lines input
      cols = length $ head $ lines input
  solve1 _ = show . minimumLoss 0 3

type PriorityQueue a = M.Map Int [a]

pInsert :: Int -> a -> PriorityQueue a -> PriorityQueue a
pInsert key value = M.insertWith (++) key [value]

pFirst :: PriorityQueue a -> Maybe a
pFirst queue = head . fst <$> M.minView queue

pDrop :: PriorityQueue a -> PriorityQueue a
pDrop queue = case M.minViewWithKey queue of
  Nothing -> queue
  Just ((_, []), new_queue) -> new_queue
  Just ((_, [_]), new_queue) -> new_queue
  Just ((k, _:_:_), _) -> M.adjust tail k queue

data Path = Path
  { pathCost :: Int
  , pathPos :: Point
  , pathDir :: Point
  , pathSteps :: Int
  } deriving (Eq, Show, Ord)

data VisitedKey = VisitedKey
  { visitedPos :: Point
  , visitedDir :: Point
  , visitedSteps :: Int
  } deriving (Show, Eq, Ord)

visitedKeyFromPath :: Path -> VisitedKey
visitedKeyFromPath (Path _ pos dir steps) = VisitedKey pos dir steps

data Status = Status
  { statusFrontier :: PriorityQueue Path
  , statusVisited :: S.Set VisitedKey
  }

minimumLoss :: Int -> Int -> Grid -> Int
minimumLoss min_forward max_forward grid = grid ! start + evalState nextIteration starting_status
  where
    (start, finish) = bounds grid
    starting_status = Status (pInsert 0 (Path 0 start down 0) $ pInsert 0 (Path 0 start right 0) mempty) mempty

    loop :: Path -> State Status Int
    loop (Path cost pos dir steps)
      | pos == finish = return cost
      | otherwise = do
        let invalid d = (d == negate d) || (d == dir && steps >= max_forward) || (d /= dir && steps < min_forward)
        let directions = filter (not . invalid) [up, down, left, right]
        let new_pos = pos + dir
        when (inRange (bounds grid) new_pos) $ do
          forM_ directions $ \direction -> do
            let new_cost = cost + grid ! new_pos
            let new_path = Path new_cost new_pos direction (if direction == dir then succ steps else 1)
            let key = visitedKeyFromPath new_path
            has <- gets $ S.member key . statusVisited
            unless has $ modify $ \(Status frontier visited) -> Status (pInsert (pathCost new_path) new_path frontier) (S.insert key visited)
        nextIteration

    nextIteration :: State Status Int
    nextIteration = do
      next <- gets $ pFirst . statusFrontier
      modify $ \(Status frontier visited) -> Status (pDrop frontier) visited
      loop $ fromJust next

instance Part2 Day17 Grid where
  parse2 = parse1
  solve2 _ = show . minimumLoss 4 10

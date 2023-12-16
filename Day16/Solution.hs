{-# OPTIONS_GHC -Wall -Wextra #-}
module Day16.Solution (Day16(..)) where

import Parts

import Data.Maybe (fromJust)

import Data.Array
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad (filterM)

data Day16 = Day16 deriving Show

data Direction = U | D | L | R
  deriving (Show, Eq, Ord)

fromDirection :: Direction -> Position
fromDirection U = Position 0 (-1)
fromDirection D = Position 0 1
fromDirection L = Position (-1) 0
fromDirection R = Position 1 0

data Position = Position Int Int
  deriving (Show, Eq, Ord, Ix)

instance Num Position where
  Position x1 y1 + Position x2 y2 = Position (x1+x2) (y1+y2)
  Position x1 y1 * Position x2 y2 = Position (x1*x2) (y1*y2)
  abs (Position x y) = Position (abs x) (abs y)
  signum (Position x y) = Position (signum x) (signum y)
  fromInteger n = Position (fromInteger n) (fromInteger n)
  negate (Position x y) = Position (negate x) (negate y)

-- '.', '-', '|', '/', '\'
type Mirror = Char

type Mirrors = Array Position Mirror

-- example should give 46
instance Part1 Day16 Mirrors where
  parse1 _ input = array (Position 1 1, Position cols rows)
    [ (Position x y, c)
    | (y, line) <- zip [1..] $ lines input
    , (x, c) <- zip [1..] line
    ]
    where
      rows = length $ lines input
      cols = length $ head $ lines input
  solve1 _ = show . energized (Beam (Position 1 1) R)

energized :: Beam -> Mirrors -> Int
energized beam = M.size . simVisited . execState (stepAllBeams [beam]) . startSim

data Beam = Beam
  { beamPos :: Position
  , beamDir :: Direction
  } deriving (Show, Eq, Ord)

passMirror :: Direction -> Mirror -> [Direction]
passMirror dir '|'
  | dir `elem` [U, D] = return dir
  | otherwise = [U, D]
passMirror dir '-'
  | dir `elem` [L, R] = return dir
  | otherwise = [L, R]
passMirror dir '/'  = return $ fromJust $ lookup dir [(D, L), (R, U), (L, D), (U, R)]
passMirror dir '\\' = return $ fromJust $ lookup dir [(D, R), (R, D), (L, U), (U, L)]
passMirror dir _ = return dir

data Sim = Sim
  { simVisited :: M.Map Position (S.Set Direction)
  , simMirrors :: Mirrors
  }

startSim :: Mirrors -> Sim
startSim = Sim mempty

stepBeam :: Beam -> State Sim [Beam]
stepBeam (Beam pos dir) = do
  already_visited <- gets $ maybe False (S.member dir) . M.lookup pos . simVisited
  if already_visited
  then return []
  else do
    modify $ \(Sim visited mirrors) -> Sim (M.insertWith S.union pos (S.singleton dir) visited) mirrors
    new_dirs <- gets $ passMirror dir . (! pos) . simMirrors
    return $ map (\dir' -> Beam (pos + fromDirection dir') dir') new_dirs

beamInBounds :: Beam -> State Sim Bool
beamInBounds (Beam pos _) = gets $ flip inRange pos . bounds . simMirrors

stepAllBeams :: [Beam] -> State Sim ()
stepAllBeams [] = return ()
stepAllBeams beams = traverse stepBeam beams >>= filterM beamInBounds . concat >>= stepAllBeams

instance Part2 Day16 Mirrors where
  parse2 = parse1
  solve2 _ mirrors = show $ maximum $ map (`energized` mirrors) beams
    where
      (Position min_x min_y, Position max_x max_y) = bounds mirrors
      b_edge = map (`Position` min_y) [min_x..max_x]
      t_edge = map (`Position` max_y) [min_x..max_x]
      l_edge = map (Position min_x) [min_y..max_y]
      r_edge = map (Position max_x) [min_y..max_y]
      positions = b_edge ++ t_edge ++ l_edge ++ r_edge
      beams = positions >>= \pos -> map (Beam pos) [U, D, L, R]

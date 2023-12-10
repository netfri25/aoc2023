{-# OPTIONS_GHC -Wall -Wextra #-}
module Day10.Solution (Day10(..)) where

import Parts

import qualified Data.Map as M
import Data.List (find)
import Data.Maybe (catMaybes, mapMaybe, listToMaybe, fromJust, fromMaybe)
import Data.Bits (xor)

data Day10 = Day10 deriving Show

up :: Position
up = Position 0 (-1)

down :: Position
down = Position 0 1

left :: Position
left = Position (-1) 0

right :: Position
right = Position 1 0

data Pipe = Start | Pipe [Position]
  deriving (Show, Eq)

data Position = Position Int Int
  deriving (Show, Eq, Ord)

posY :: Position -> Int
posY (Position _ y) = y

posX :: Position -> Int
posX (Position x _) = x

instance Num Position where
  Position x1 y1 + Position x2 y2 = Position (x1+x2) (y1+y2)
  Position x1 y1 * Position x2 y2 = Position (x1*x2) (y1*y2)
  abs (Position x y) = Position (abs x) (abs y)
  signum (Position x y) = Position (signum x) (signum y)
  fromInteger n = Position (fromInteger n) (fromInteger n)
  negate (Position x y) = Position (negate x) (negate y)

readPipe :: Char -> Maybe Pipe
readPipe = flip lookup
  [ ('|',Pipe [up, down])
  , ('-',Pipe [left, right])
  , ('L',Pipe [up, right])
  , ('J',Pipe [left, up])
  , ('7',Pipe [left, down])
  , ('F',Pipe [down, right])
  , ('S',Start)
  ]

parsePipes :: String -> [(Position, Pipe)]
parsePipes input = catMaybes
  [ (Position col row,) <$> readPipe c
  | (row, l) <- zip [1..] $ lines input
  , (col, c) <- zip [1..] l
  ]

stepFrom :: Position -> Grid Pipe -> Grid Int -> Position -> Maybe (Grid Int, Position)
stepFrom came_from pipes dists from = do
  Pipe directions <- M.lookup from pipes
  to <- find (/= came_from) $ map (+ from) directions
  dist <- succ <$> M.lookup from dists
  return (M.insertWith (\ _ x -> x) to dist dists, to)

stepToStart :: Position -> Grid Pipe -> Grid Int -> Position -> Maybe (Grid Int)
stepToStart came_from pipes dists pos =
  case M.lookup pos pipes of
    Nothing -> Nothing
    Just Start -> Just dists
    _ -> stepFrom came_from pipes dists pos >>= uncurry (stepToStart pos pipes)

type Grid = M.Map Position

connectedTo :: Grid Pipe -> Position -> Position -> Bool
connectedTo pipes to from =
  case M.lookup from pipes of
    Just (Pipe dirs) -> to `elem` map (+from) dirs
    _ -> False

mainLoop :: Grid Pipe -> Maybe (Grid Int)
mainLoop pipes = listToMaybe $ mapMaybe (stepToStart start_pos pipes <$> initialDists <*> id) starts
  where
    start_pos = fst $ head $ filter ((== Start) . snd) $ M.assocs pipes
    starts = filter (connectedTo pipes start_pos) $ map (+ start_pos) [up, down, left, right]

    initialDists :: Position -> Grid Int
    initialDists pos = M.fromList [(start_pos, 0), (pos, 1)]

instance Part1 Day10 (Grid Pipe) where
  parse1 _ = M.fromList . parsePipes
  solve1 _ = show . fmap (flip div 2 . M.size ) . mainLoop

replaceStart :: Grid Pipe -> Grid Pipe
replaceStart pipes = M.insert start_pos start' pipes
  where
    start_pos = fst $ fromJust $ find ((==) Start . snd) $ M.assocs pipes
    start' = Pipe $ filter (connectedTo pipes start_pos . (start_pos +)) [up, down, left, right]

showPipe :: Pipe -> Maybe Char
showPipe = flip lookup
  [ (Pipe [up, down],'|')
  , (Pipe [left, right],'-')
  , (Pipe [up, right],'L')
  , (Pipe [left, up],'J')
  , (Pipe [left, down],'7')
  , (Pipe [down, right],'F')
  ]

showPipesGrid :: Grid Pipe -> [String]
showPipesGrid pipes = do
  y <- [min_y..max_y]
  return $ do
    x <- [min_x..max_x]
    let pos = Position x y
    return $ fromMaybe ' ' $ M.lookup pos pipes >>= showPipe
  where
    min_x = minimum $ map posX $ M.keys pipes
    min_y = minimum $ map posY $ M.keys pipes
    max_x = maximum $ map posX $ M.keys pipes
    max_y = maximum $ map posY $ M.keys pipes

countInside :: String -> Int
countInside = countInside' False []
  where
    countInside' :: Bool -> [Char] -> String -> Int
    countInside' _ _ [] = 0
    countInside' inside stack (c:cs) =
      case c of
        '|' -> countInside' (not inside) stack cs
        '-' -> countInside' inside stack cs
        'L' -> countInside' inside (c:stack) cs
        'F' -> countInside' inside (c:stack) cs
        '7' ->
          let inside' = (head stack == 'L') `xor` inside
           in countInside' inside' (tail stack) cs
        'J' ->
          let inside' = (head stack == 'F') `xor` inside
           in countInside' inside' (tail stack) cs
        ' ' -> fromEnum inside + countInside' inside stack cs
        _ -> undefined

instance Part2 Day10 [String] where
  parse2 day input = showPipesGrid $ fromJust main_pipes
    where
      pipes = parse1 day input
      main_loop = mainLoop pipes
      main_pipes = replaceStart . M.intersectionWith const pipes <$> main_loop
  solve2 _ = show . sum . map countInside

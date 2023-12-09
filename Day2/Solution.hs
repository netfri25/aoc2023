{-# OPTIONS_GHC -Wall -Wextra #-}
module Day2.Solution (Day2(..)) where

import Parts
import Parser

import Control.Applicative (Alternative(..))
import Control.Monad (mfilter)
import qualified Data.Map.Strict as M

data Day2 = Day2 deriving Show

data Color = Red | Green | Blue deriving (Show, Eq, Ord)
type Cube = Color
type CubeSet = M.Map Cube Int
data Game = Game
  { gameId :: Int
  , gameSets :: [CubeSet]
  } deriving Show

parseGame :: Parser String Game
parseGame = Game <$> gameNum <* ws <*> gameSets
  where
    gameNum = seqP "Game" *> ws *> numP <* eqP ':'
    gameSets = sepBy (ws *> eqP ';' <* ws) parseSet

parseSet :: Parser String CubeSet
parseSet = M.fromList <$> sepBy (ws *> eqP ',' <* ws) (flip (,) <$> numP <* ws <*> parseColor)

parseColor :: Parser String Color
parseColor = red <|> green <|> blue
  where
    red = Red <$ seqP "red"
    green = Green <$ seqP "green"
    blue = Blue <$ seqP "blue"

instance Part1 Day2 [Game] where
  parse1 _ = runParser $ mfilter (not . null) $ sepBy ws parseGame
  solve1 _ = Result . sum . map gameId . filter canPlayGame
    where
      canPlayGame (Game _ cubeSets) = all canPlaySet cubeSets
      canPlaySet = and . M.intersectionWith (>=) maxCubes
      maxCubes = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

instance Part2 Day2 [Game] where
  parse2 = parse1
  solve2 _ = Result . sum . map (product . M.unionsWith max . gameSets)

{-# LANGUAGE TypeFamilies #-}
module Day2.Solution (Day2(..)) where

import Parts
import Parser

import Control.Applicative (Alternative(..))
import Control.Monad (mfilter)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

data Day2 = Day2 deriving Show

instance InputPath Day2 where
  examplePath = const "Day2/example.txt"
  inputPath = const "Day2/input.txt"

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
    gameNum = listP "Game" *> ws *> numP <* eqP ':'
    gameSets = sepBy (ws *> eqP ';' <* ws) parseSet

parseSet :: Parser String CubeSet
parseSet = M.fromList <$> sepBy (ws *> eqP ',' <* ws) (flip (,) <$> numP <* ws <*> parseColor)

parseColor :: Parser String Color
parseColor = red <|> green <|> blue
  where
    red = Red <$ listP "red"
    green = Green <$ listP "green"
    blue = Blue <$ listP "blue"

instance Part1 Day2 where
  type Input Day2 = [Game]
  parse1 _ = maybe [] fst . runParserT (mfilter (not . null) $ sepBy ws parseGame)
  solve1 _ = Result . sum . map gameId . filter canPlayGame
    where
      canPlayGame (Game _ cubeSets) = all canPlaySet cubeSets
      canPlaySet = and . M.intersectionWith (>=) maxCubes
      maxCubes = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

instance Part2 Day2 where
  parse2 = parse1
  solve2 _ = Result . sum . map (product . M.unionsWith max . gameSets)

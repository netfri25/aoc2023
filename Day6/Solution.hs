{-# OPTIONS_GHC -Wall -Wextra #-}
module Day6.Solution (Day6(..)) where

import Parts
import Parser

import Text.Read (readMaybe)
import Data.Maybe (fromJust)

data Day6 = Day6 deriving Show

type Time = Int
type Distance = Int

data Race = Race
  { raceTime :: Time
  , raceHighscore :: Distance
  } deriving Show

timePrefix :: Parser String String
timePrefix = seqP "Time:"

parseTimes :: Parser String [Time]
parseTimes = timePrefix *> ws *> sepBy ws numP

distPrefix :: Parser String String
distPrefix = seqP "Distance:"

parseDists :: Parser String [Distance]
parseDists = distPrefix *> ws *> sepBy ws numP

parseRaces :: Parser String [Race]
parseRaces = zipWith Race <$> parseTimes <* ws <*> parseDists

race :: Time -> Time -> Distance
race total held = held * (total - held)

wins :: Race -> Int
wins (Race total high) = length $ takeWhile (>high) $ dropWhile (<=high) $ map (race total) [0..total]

instance Part1 Day6 [Race] where
  parse1 _ = maybe [] fst . runParserT parseRaces
  solve1 _ = Result . product . map wins

parseSingleRace :: Parser String Race
parseSingleRace = Race <$> race_time <* ws <*> race_highscore
  where
    nums_as_num = sepBy ws numP >>= lift . readMaybe . concatMap show
    race_time      = timePrefix *> ws *> nums_as_num
    race_highscore = distPrefix *> ws *> nums_as_num

instance Part2 Day6 Race where
  parse2 _ = fst . fromJust . runParserT parseSingleRace
  solve2 day r = solve1 day [r]

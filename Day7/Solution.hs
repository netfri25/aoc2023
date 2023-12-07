{-# OPTIONS_GHC -Wall -Wextra #-}
module Day7.Solution (Day7(..)) where

import Parts
import Parser

import Data.Word (Word8)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Data.List (sortOn, groupBy, sort, group)
import Data.Function (on)
import Data.Bifunctor (second)

data Day7 = Day7 deriving Show

instance InputPath Day7 where
  examplePath = const "Day7/example.txt"
  inputPath = const "Day7/input.txt"

newtype Card = Card Word8
  deriving (Show, Eq, Ord)

type CardTable = [(Char, Card)]

cardTable :: CardTable
cardTable = map (second Card) $ map ((,) <$> head . show <*> id) [2..9] ++ [('T', 10), ('J', 11) , ('Q', 12) , ('K', 13) , ('A', 14)]

toCard :: Char -> Maybe Card
toCard = flip lookup cardTable

parseCard :: Parser String Card
parseCard = nextP >>= lift . toCard

type Hand = [Card]

parseHand :: Parser String Hand
parseHand = replicateM 5 parseCard

data Strength
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | FullHouse
  | FourKind
  | FiveKind
  deriving (Show, Eq, Ord)

allEq :: Eq a => [a] -> Bool
allEq = and . (zipWith (==) <$> drop 1 <*> init)

handStrength :: Ord a => [a] -> Strength
handStrength hand
  | allEq hand = FiveKind
  | length (last grouped) == pred (length hand) = FourKind
  | length grouped == 2 = FullHouse
  | length (last grouped) == length hand - 2 = ThreeKind
  | length (filter ((==2) . length) grouped) == 2 = TwoPair
  | length (last grouped) == length hand - 3 = OnePair
  | otherwise = HighCard
  where
    -- sorted from the shortest group to the longest group
    grouped = sortOn length $ group $ sort hand

type Bid = Int

parseBid :: Parser String Bid
parseBid = numP

data Bet = Bet
  { betHand :: Hand
  , betBid :: Bid
  } deriving Show

parseBet :: Parser String Bet
parseBet = Bet <$> parseHand <* ws <*> parseBid

totalWins :: (Bet -> Strength) -> [Bet] -> Int
totalWins strength = sum . zipWith (*) [1..] . map betBid . concatMap (sortOn betHand) . groupBy ((==) `on` strength) . sortOn strength

instance Part1 Day7 [Bet] where
  parse1 _ = fst . fromJust . runParserT (sepBy ws parseBet)
  solve1 _ = Result . totalWins (handStrength . betHand)

change :: Eq a => a -> a -> [a] -> [a]
change _ _ [] = []
change from to (x:xs)
  | x == from = to : change from to xs
  | otherwise = x  : change from to xs

oldJ :: Card
oldJ = fromJust $ lookup 'J' cardTable

newJ :: Card
newJ = Card 1

instance Part2 Day7 [Bet] where
  parse2 day = map (\(Bet hand bid) -> Bet (change oldJ newJ hand) bid) . parse1 day
  solve2 _ = Result . totalWins (handStrength . filter (/=newJ) . betHand)

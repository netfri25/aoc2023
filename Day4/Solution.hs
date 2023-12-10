{-# OPTIONS_GHC -Wall -Wextra #-}
module Day4.Solution (Day4(..)) where

import Parts
import Parser

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Monad.State
import Data.Bifunctor (first)

data Day4 = Day4 deriving Show

data Card = Card
  { cardId :: Int
  , cardNums :: IS.IntSet
  , cardWinningNumbers :: IS.IntSet
  } deriving Show

parseNums :: Parser String IS.IntSet
parseNums = IS.fromList <$> sepBy ws numP

parseCard :: Parser String Card
parseCard = Card <$> card_id <* ws <*> parseNums <* ws <* eqP '|' <* ws <*> parseNums <* ws
  where card_id = seqP "Card" *> ws *> numP <* eqP ':' <* ws

winning :: Card -> IS.IntSet
winning = IS.intersection <$> cardNums <*> cardWinningNumbers

cardWins :: Card -> Int
cardWins = IS.size . winning

cardValue :: Card -> Int
cardValue = safePow 2 . pred . cardWins

safePow :: Int -> Int -> Int
safePow _ e | e < 0 = 0
safePow n e = n^e

instance Part1 Day4 [Card] where
  parse1 _ = runParser (sepBy ws parseCard)
  solve1 _ = show . sum . map cardValue

type Count = Int
type Simulation = State (IM.IntMap (Count, Card))

addCards :: Int -> Int -> Simulation ()
addCards amount card_id = modify $ IM.adjust (first (+ amount)) card_id

evalCard :: Int -> Simulation ()
evalCard card_id = do
  (amount, card) <- gets (IM.! card_id)
  let wins = cardWins card
  let cards_got = take wins [succ card_id..]
  mapM_ (addCards amount) cards_got

instance Part2 Day4 [Card] where
  parse2 = parse1
  solve2 _ cards = show $ sum $ map fst $ IM.elems $ flip execState cards_map $ mapM_ (evalCard . cardId) cards
    where cards_map = IM.fromList $ zip (map cardId cards) $ map (1,) cards

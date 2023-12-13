{-# OPTIONS_GHC -Wall -Wextra #-}
module Day3.Solution (Day3(..)) where

import Parts
import Parser

import Data.List (uncons)
import Control.Monad.State (gets)
import Data.Char (isDigit, isSpace)
import Control.Applicative (Alternative(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Either (isLeft, isRight, rights)

data Day3 = Day3 deriving Show

type ItemKind = Either Char Int
data Position = Position !Int !Int deriving (Show, Eq, Ord)
type Item = (Position, ItemKind)

data ItemsInput = MkInput
  { inputRow  :: !Int
  , inputCol  :: !Int
  , inputText :: !String
  } deriving Show

instance Input ItemsInput Char where
  inputNext (MkInput row col text) = do
    (c, cs) <- uncons text
    let input' = if  c == '\n'  then  MkInput (row+1) 1  else  MkInput row (col+1)
    return (c, input' cs)

parserPos :: Parser ItemsInput Position
parserPos = gets $ Position <$> inputRow <*> inputCol

parseSymbol :: Parser ItemsInput ItemKind
parseSymbol = Left <$> ifP (\c -> not (isDigit c) && c /='.')

parseNumber :: Parser ItemsInput ItemKind
parseNumber = Right <$> numP

parseItem :: Parser ItemsInput Item
parseItem = (,) <$> parserPos <*> (parseSymbol <|> parseNumber)

isPart :: S.Set Position -> Item -> Bool
isPart symbols (Position row col, Right n) = any (`S.member` symbols) $ do
  c <- [col-1 .. col + length (show n)]
  r <- [row-1..row+1]
  return $ Position r c
isPart _ _ = undefined

getParts :: M.Map Position ItemKind -> [Item]
getParts items = parts
  where
    numbers = filter (isRight . snd) $ M.assocs items
    symbols = S.fromList $ map fst $ filter (isLeft . snd) $ M.assocs items
    parts = filter (isPart symbols) numbers

instance Part1 Day3 (M.Map Position ItemKind) where
  parse1 _ = M.fromList . runParser (skip *> many (skip *> parseItem <* skip)) . MkInput 1 1
    where skip = spanP (\c -> isSpace c || c == '.')
  solve1 _ = show . sum . rights . map snd . getParts

partsNear :: Position -> [Item] -> [Item]
partsNear (Position prow pcol) = filter $ any (`S.member` positions) . allPositions
  where
    positions = S.fromList [Position r c | r <- [prow-1..prow+1], c <- [pcol-1..pcol+1]]
    allPositions (Position row col, Right n) = Position row <$> [col..col + length (show n) - 1]
    allPositions _ = undefined

-- lazily check the length
lengthIs :: Int -> [a] -> Bool
lengthIs 0 [] = True
lengthIs 0 _ = False
lengthIs _ [] = False
lengthIs n xs = lengthIs (pred n) (tail xs)

instance Part2 Day3 (M.Map Position ItemKind) where
  parse2 = parse1
  solve2 _ items = show $ sum $ map (product . rights . map snd) gearsParts
    where
      parts = getParts items
      stars = M.keys $ M.filter (==Left '*') items
      gearsParts = filter (lengthIs 2) $ map (`partsNear` parts) stars

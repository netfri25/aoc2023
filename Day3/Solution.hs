{-# OPTIONS_GHC -Wall -Wextra #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Day3.Solution (Day3(..)) where

import Parts
import Parser
import Data.List (uncons)
import Control.Monad.State (gets)
import Data.Char (isDigit, isSpace)
import Control.Applicative (Alternative(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Day3 = Day3 deriving Show

data ItemKind = Symbol Char | Number !Int deriving (Show, Eq)
data Position = Position !Int !Int deriving (Show, Eq, Ord)
type Item = (Position, ItemKind)

isSymbol :: ItemKind -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isNumber :: ItemKind -> Bool
isNumber = not . isSymbol

fromNumber :: ItemKind -> Int
fromNumber (Number n) = n
fromNumber _ = undefined

data ItemsInput = MkInput
  { inputRow  :: !Int
  , inputCol  :: !Int
  , inputText :: !String
  } deriving Show

instance Parser.Input ItemsInput Char where
  inputNext (MkInput row col text) = do
    (c, cs) <- uncons text
    let input' = if  c == '\n'  then  MkInput (row+1) 1 cs  else  MkInput row (col+1) cs
    return (c, input')

parserPos :: Parser ItemsInput Position
parserPos = gets $ Position <$> inputRow <*> inputCol

parseSymbol :: Parser ItemsInput ItemKind
parseSymbol = Symbol <$> ifP ((&&) <$> not . isDigit <*> (/='.'))

parseNumber :: Parser ItemsInput ItemKind
parseNumber = Number <$> numP

parseItem :: Parser ItemsInput Item
parseItem = (,) <$> parserPos <*> (parseSymbol <|> parseNumber)

isPart :: S.Set Position -> Item -> Bool
isPart symbols (Position row col, Number n) = any (`S.member` symbols) $ do
  c <- [col-1 .. col + length (show n)]
  r <- [row-1..row+1]
  return $ Position r c
isPart _ _ = undefined

getParts :: M.Map Position ItemKind -> [Item]
getParts items = parts
  where
    numbers = filter (isNumber . snd) $ M.assocs items
    symbols = S.fromList $ map fst $ filter (isSymbol . snd) $ M.assocs items
    parts = filter (isPart symbols) numbers

instance Part1 Day3 (M.Map Position ItemKind) where
  parse1 _ = M.fromList . runParser (skip *> many (skip *> parseItem <* skip)) . MkInput 1 1
    where skip = spanP (\c -> isSpace c || c == '.')
  solve1 _ = show . sum . map (fromNumber . snd) . getParts

partsNear :: Position -> [Item] -> [Item]
partsNear (Position prow pcol) = filter (any (`elem` positions) . allPositions)
  where
    positions = [Position r c | r <- [prow-1..prow+1], c <- [pcol-1..pcol+1]]
    allPositions (Position row col, Number n) = Position row <$> [col..col + length (show n) - 1]
    allPositions _ = undefined

lengthIs :: Int -> [a] -> Bool
lengthIs 0 [] = True
lengthIs 0 _ = False
lengthIs _ [] = False
lengthIs n xs = lengthIs (pred n) (tail xs)

instance Part2 Day3 (M.Map Position ItemKind) where
  parse2 = parse1
  solve2 _ items = show $ sum $ map (product . map (fromNumber . snd)) gearsParts
    where
      parts = getParts items
      stars = map fst $ filter (\case (snd -> Symbol '*') -> True; _ -> False) $ M.assocs items
      gearsParts = filter (lengthIs 2) $ map (`partsNear` parts) stars

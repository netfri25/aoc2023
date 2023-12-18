{-# OPTIONS_GHC -Wall -Wextra #-}
module Day18.Solution (Day18(..)) where

import Parts
import Parser

import Control.Monad (replicateM)
import Data.Char (isHexDigit, digitToInt)
import Data.Maybe (fromJust)

data Day18 = Day18 deriving Show

data Point = Point Int Int
  deriving (Show, Eq, Ord)

pointX :: Point -> Int
pointX (Point x _) = x

pointY :: Point -> Int
pointY (Point _ y) = y

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

type Color = String

parseColor :: Parser String Color
parseColor = eqP '#' *> replicateM 6 hexP

hexP :: Input i Char => Parser i Char
hexP = ifP isHexDigit

type Direction = Point

parseDirection :: Parser String Direction
parseDirection = nextP >>= lift . flip lookup [('U', up), ('D', down), ('L', left), ('R', right)]

parseOffset :: Parser String Point
parseOffset = (\point delta -> point * fromIntegral delta) <$> parseDirection <* ws <*> numP

shoelace :: [Point] -> Int
shoelace points = abs $ div area 2
  where
    xs = map pointX points
    ys = map pointY points
    pos_area = sum $ zipWith (*) xs (tail ys)
    neg_area = sum $ zipWith (*) ys (tail xs)
    area = pos_area - neg_area

instance Part1 Day18 [Point] where
  parse1 _ = runParser $ sepBy ws (parseOffset <* ws <* eqP '(' <* parseColor <* eqP ')')
  solve1 _ offsets = show $ area + perimeter `div` 2 + 1
    where
      points = scanl (+) 1 offsets
      area = shoelace points
      Point len_x len_y = sum $ map abs offsets
      perimeter = len_x + len_y

hexDigitToInt :: Char -> Int
hexDigitToInt digit = fromJust $ lookup digit (zip ['0'..'9'] [0..9] ++ zip ['a'..'f'] [10..16])

parseFromColor :: Color -> Point
parseFromColor letters = fromIntegral num * dir
  where
    dir = [right, down, left, up] !! digitToInt (last letters)
    num_hex = take 5 letters
    num_digits = map hexDigitToInt num_hex
    base16 = iterate (*16) 1
    num = sum $ zipWith (*) base16 (reverse num_digits)

instance Part2 Day18 [Point] where
  parse2 _ = runParser $ sepBy ws (fmap parseFromColor $ parseOffset *> ws *> eqP '(' *> parseColor <* eqP ')')
  solve2 = solve1

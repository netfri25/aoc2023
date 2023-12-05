{-# OPTIONS_GHC -Wall -Wextra #-}
module Day5.Solution (Day5(..)) where

import Parts
import Parser
import Data.Char (isAlpha)
import Data.Maybe (fromJust, mapMaybe, listToMaybe, fromMaybe)

data Day5 = Day5 deriving Show

instance InputPath Day5 where
  examplePath = const "Day5/example.txt"
  inputPath = const "Day5/input.txt"

data Range = Range
  { rangeStart :: Int
  , rangeEnd   :: Int
  } deriving Show

parseRange :: Parser String Range
parseRange = (\start off -> Range start (start + off - 1)) <$> numP <* ws <*> numP

inRange :: Range -> Int -> Bool
inRange (Range start end) n = n >= start && n <= end

validRange :: Range -> Bool
validRange (Range start end) = start <= end

rangeIntersection :: Range -> Range -> Range
rangeIntersection (Range start1 end1) (Range start2 end2) = Range (max start1 start2) (min end1 end2)

rangeShift :: Int -> Range -> Range
rangeShift off (Range start end) = Range (start + off) (end + off)

data Conversion = Conversion
  { convDestStart :: Int
  , convSourceStart :: Int
  , convLength :: Int
  } deriving Show

convSourceEnd :: Conversion -> Int
convSourceEnd c = convSourceStart c + convLength c - 1

convSourceRange :: Conversion -> Range
convSourceRange = Range <$> convSourceStart <*> convSourceEnd

convOffset :: Conversion -> Int
convOffset = (-) <$> convDestStart <*> convSourceStart

parseConversion :: Parser String Conversion
parseConversion = Conversion <$> numP <* ws <*> numP <* ws <*> numP

data Map = Map
  { mapFrom :: String
  , mapTo :: String
  , mapConversions :: [Conversion]
  } deriving Show

parseMap :: Parser String Map
parseMap = Map <$> spanP isAlpha <* listP "-to-" <*> spanP isAlpha <* ws <* listP "map:" <* ws <*> sepBy ws parseConversion

parseSeeds1 :: Parser String [Int]
parseSeeds1 = listP "seeds:" *> ws *> sepBy ws numP

convert :: Int -> Conversion -> Maybe Int
convert from conv
  | inRange (convSourceRange conv) from = Just to
  | otherwise = Nothing
  where
    diff = convOffset conv
    to = diff + from

convertByMap :: Int -> Map -> Int
convertByMap from (Map {mapConversions}) = fromMaybe from $ listToMaybe $ mapMaybe (convert from) mapConversions

instance Part1 Day5 ([Int], [Map]) where
  parse1 _ = fst . fromJust . runParserT ((,) <$> parseSeeds1 <* ws <*> sepBy ws parseMap)
  solve1 _ (seeds, maps) = Result $ minimum $ map (flip (foldl convertByMap) maps) seeds

parseSeeds2 :: Parser String [Range]
parseSeeds2 = listP "seeds:" *> ws *> sepBy ws parseRange

convertRange :: Range -> [Conversion] -> [Range]
convertRange r [] = return r -- rangeShift (convOffset conv) $ rangeIntersection r (convSourceRange conv)
convertRange r (conv:convs)
  | not $ rangesOverlap (convSourceRange conv) r = convertRange r convs
  | otherwise =
    let intersection = rangeShift (convOffset conv) $ rangeIntersection r (convSourceRange conv)
     in intersection : rest
  where
    rest = case rangesDiff r (convSourceRange conv) of
      (Nothing, Nothing) -> []
      (Just r', Nothing) -> convertRange r' convs
      (Just r', Just r'') -> convertRange r' convs ++ convertRange r'' convs
      (Nothing, Just r') -> convertRange r' convs

rangesDiff :: Range -> Range -> (Maybe Range, Maybe Range)
rangesDiff r1@(Range start1 end1) r2@(Range start2 end2)
  | inRange r2 start1 && inRange r2 end1 = (Nothing, Nothing)
  | inRange r1 (start2 - 1) && inRange r1 (end2 + 1) = (Just (r1 {rangeEnd = start2 - 1}), Just (r1 {rangeStart = end2 + 1}))
  | inRange r1 (start2 - 1) = (Just (r1 {rangeEnd = start2 - 1}), Nothing)
  | inRange r1 (end2 + 1) = (Just (r1 {rangeStart = end2 + 1}), Nothing)
  | otherwise = (Just r1, Nothing)

convertRangesByMap :: [Range] -> Map -> [Range]
convertRangesByMap rs (Map {mapConversions}) = trimRanges $ concatMap (filter validRange . flip convertRange mapConversions) rs

rangesOverlap :: Range -> Range -> Bool
rangesOverlap r1@(Range start1 end1) r2@(Range start2 end2)
  | inRange r1 start2 = True
  | inRange r1 end2 = True
  | inRange r2 start1 = True
  | inRange r2 end1 = True
  | otherwise = False

rangesAdjacent :: Range -> Range -> Bool
rangesAdjacent (Range start end) = rangesOverlap (Range (start - 1) (end + 1))

rangesUnion :: Range -> Range -> Range
rangesUnion (Range start1 end1) (Range start2 end2) = Range (min start1 start2) (max end1 end2)

trimRanges :: [Range] -> [Range]
trimRanges =
  foldl
    ( \trimmed range ->
        let adjacent = filter (rangesAdjacent range) trimmed
            notAdjacent = filter (not . rangesAdjacent range) trimmed
            merged = foldl rangesUnion range adjacent
         in (merged : notAdjacent)
    )
    []

instance Part2 Day5 ([Range], [Map]) where
  parse2 _ = fst . fromJust . runParserT ((,) <$> parseSeeds2 <* ws <*> sepBy ws parseMap)
  solve2 _ (seeds, maps) = Result $ minimum $ map rangeStart $ foldl convertRangesByMap seeds maps

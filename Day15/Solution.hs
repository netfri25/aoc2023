{-# OPTIONS_GHC -Wall -Wextra #-}
module Day15.Solution (Day15(..)) where

import Parts
import Parser

import Data.Array
import Control.Monad.State

import Data.Char (isSpace, isAlpha)
import Control.Applicative ((<|>))
import Data.Word (Word8)

data Day15 = Day15 deriving Show

hash :: String -> Int
hash = fromIntegral . foldl (\acc c -> (acc + fromEnum c) * 17 `mod` 256) 0

instance Part1 Day15 [String] where
  parse1 _ = runParser $ sepBy (eqP ',') $ spanP (\c -> c /= ',' && not (isSpace c))
  solve1 _ = show . sum . map hash

data Inst = Rem String | Set String Int
  deriving Show

parseInst :: Parser String Inst
parseInst = parseRemInst <|> parseSetInst

parseRemInst :: Parser String Inst
parseRemInst = Rem <$> parseLens <* eqP '-'

parseSetInst :: Parser String Inst
parseSetInst = Set <$> parseLens <* eqP '=' <*> numP

parseLens :: Parser String String
parseLens = spanP isAlpha

data Lens = Lens
  { lensName :: String
  , lensFL :: Int
  } deriving Show

type Box = [Lens]
type Boxs = Array Word8 Box

runInst :: Inst -> State Boxs ()
runInst (Rem lens) = modify $ applyTo (fromIntegral $ hash lens) $ deleteWhen $ (==lens) . lensName
runInst (Set lens value) = modify $ applyTo (fromIntegral $ hash lens) $ alterWhen ((==lens) . lensName) $ Lens lens value

applyTo :: Ix i => i -> (a -> a) -> Array i a -> Array i a
applyTo i alter arr = arr // [(i, alter $ arr ! i)]

deleteWhen :: (a -> Bool) -> [a] -> [a]
deleteWhen _ [] = []
deleteWhen f (x:xs)
  | f x = xs
  | otherwise = x : deleteWhen f xs

alterWhen :: (a -> Bool) -> a -> [a] -> [a]
alterWhen _ new [] = [new]
alterWhen f new (x:xs)
  | f x = new : xs
  | otherwise = x : alterWhen f new xs

initialBoxs :: Boxs
initialBoxs = listArray (0, 255) $ repeat []

focusingPower :: Boxs -> Int
focusingPower = sum . map (uncurry boxPower) . assocs
  where
    boxPower :: Word8 -> Box -> Int
    boxPower mult = (* succ (fromIntegral mult)) . sum . zipWith (*) [1..] . map lensFL

instance Part2 Day15 [Inst] where
  parse2 _ = runParser $ sepBy (eqP ',') parseInst
  solve2 _ = show . focusingPower . flip execState initialBoxs . mapM_ runInst

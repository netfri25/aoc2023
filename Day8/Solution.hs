{-# OPTIONS_GHC -Wall -Wextra #-}
module Day8.Solution (Day8(..)) where

import Parts
import Parser

import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Control.Applicative (many)
import Data.Maybe (fromJust, catMaybes, isJust)

data Day8 = Day8 deriving Show

data Conn = Conn
  { connLeft :: String
  , connRight :: String
  } deriving Show

parseConnName :: Parser String String
parseConnName = spanP isAlphaNum

parseConn :: Parser String Conn
parseConn = Conn <$> (eqP '(' *> parseConnName) <* ws <* eqP ',' <* ws <*> parseConnName <* ws <* eqP ')'

data Inst = L | R deriving (Show, Eq)

parseInst :: Input i Char => Parser i Inst
parseInst = nextP >>= lift . flip lookup [('L', L), ('R', R)]

parseInsts :: Input i Char => Parser i [Inst]
parseInsts = many parseInst

type Graph = M.Map String Conn

parseGraphNode :: Parser String (String, Conn)
parseGraphNode = (,) <$> parseConnName <* ws <* eqP '=' <* ws <*> parseConn

parseGraph :: Parser String Graph
parseGraph = M.fromList <$> sepBy ws parseGraphNode

runInst :: Inst -> Conn -> String
runInst L = connLeft
runInst R = connRight

path :: [Inst] -> Graph -> String -> Maybe [String]
path insts graph conn_name = not_single $ catMaybes $ takeWhile isJust $ scanl next (Just conn_name) insts
  where
    not_single :: [String] -> Maybe [String]
    not_single [_] = Nothing
    not_single xs = Just xs

    next :: Maybe String -> Inst -> Maybe String
    next mname inst = fmap (runInst inst) $ mname >>= flip M.lookup graph

pathLen :: [Inst] -> Graph -> (String -> Bool) -> String -> Maybe Int
pathLen insts graph continue_if = fmap (length . takeWhile continue_if) . path insts graph

instance Part1 Day8 ([Inst], Graph) where
  parse1 _ = fst . fromJust . runParserT ((,) <$> (cycle <$> parseInsts) <* ws <*> parseGraph)
  solve1 _ (insts, graph) = Result $ pathLen insts graph (/="ZZZ") "AAA"

instance Part2 Day8 ([Inst], Graph) where
  parse2 = parse1
  solve2 _ (insts, graph) = Result $ foldl1 lcm <$> traverse (pathLen insts graph $ not . is_end) starts
    where
      is_end = (=='Z') . last
      is_start = (=='A') . last
      starts = filter is_start $ M.keys graph

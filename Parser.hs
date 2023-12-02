{-# OPTIONS_GHC -Wall -Wextra #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parser
  ( Parser
  , Input(..)
  , runParserT
  , nextP
  , ifP
  , eqP
  , listP
  , spanP
  , sepBy
  , ws
  , numP
  ) where

import Control.Monad.State
import Data.List (uncons)
import Data.Char (isSpace, isDigit)
import Control.Monad (mfilter)
import Control.Applicative (Alternative(..))
import Text.Read (readMaybe)

class Input i c | i -> c where
  inputNext :: i -> Maybe (c, i)

instance Input [a] a where
  inputNext = uncons

type Parser i a = StateT i Maybe a

runParserT :: Parser i a -> i -> Maybe (a, i)
runParserT = runStateT

nextP :: Input i c => Parser i c
nextP = StateT inputNext

ifP :: Input i c => (c -> Bool) -> Parser i c
ifP = flip mfilter nextP

optP :: Input i c => Parser i c -> Parser i (Maybe c)
optP p = StateT $ \i -> case runParserT p i of
  Nothing -> return (Nothing, i)
  Just (r, i') -> return (Just r, i')

eqP :: (Input i c, Eq c) => c -> Parser i c
eqP c = ifP (==c)

listP :: (Input i c, Eq c, Traversable t) => t c -> Parser i (t c)
listP = traverse eqP

spanP :: Input i c => (c -> Bool) -> Parser i [c]
spanP = many . ifP

sepBy :: Parser i a -> Parser i b -> Parser i [b]
sepBy sep parser = (:) <$> parser <*> many (sep *> parser) <|> pure []

ws :: Input i Char => Parser i String
ws = spanP isSpace

numP :: Input i Char => Parser i Int
numP = maybe id (const negate) <$> optP (eqP '-') <*> (spanP isDigit >>= lift . readMaybe)

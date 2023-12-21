{-# OPTIONS_GHC -Wall -Wextra #-}
module Day20.Solution (Day20(..)) where

import Parts
import Parser

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Control.Monad (replicateM_)
import Debug.Trace

data Day20 = Day20 deriving Show

data Kind
  = Broadcast
  | FlipFlop Bool
  | Conjunction (M.Map ModName Bool)
  deriving Show

type ModName = String

data Module = Module
 { kind :: Kind
 , name :: ModName
 , dests :: [ModName]
 } deriving Show

parseModuleName :: Parser String ModName
parseModuleName = spanP isAlpha

parseModule :: Parser String Module
parseModule = (broadcast <|> other_module) <* ws <* seqP "->" <* ws <*> sends_to
  where
    broadcast = Module Broadcast "broadcaster" <$ seqP "broadcaster"
    other_module = conjunction <|> flip_flop
    conjunction = Module (Conjunction mempty) <$> (eqP '&' *> parseModuleName)
    flip_flop   = Module (FlipFlop False)     <$> (eqP '%' *> parseModuleName)
    sends_to = sepBy (eqP ',' *> ws) parseModuleName

type Modules = M.Map ModName Module

data Message = Message
  { from  :: ModName
  , dest  :: ModName
  , pulse :: Bool
  } deriving Show

parseAllModules :: Parser String Modules
parseAllModules = M.fromList . map (\m -> (name m, m)) <$> sepBy (eqP '\n') parseModule

data Pulses = Pulses
  { queue :: Seq.Seq Message
  , counts :: M.Map Bool Int -- lmao
  , mods :: Modules
  }

-- appends to the start
appendMessage :: Message -> State Pulses ()
appendMessage msg = modify (\(Pulses {queue, counts, mods}) -> Pulses (msg Seq.<| queue) (M.insertWith (+) (pulse msg) 1 counts) mods)

getMessage :: State Pulses (Maybe Message)
getMessage = do
  pulses <- gets queue
  case pulses of
    Seq.Empty -> return Nothing
    before Seq.:|> last_message -> modify (\p -> p { queue = before }) >> return (Just last_message)

runModule :: Module -> Message -> (Module, [Message])
runModule m@(Module {kind, name, dests}) (Message {from, pulse}) =
  case kind of
    Broadcast -> (m, send_all False)
    FlipFlop is_on -> if pulse then (m, []) else (m { kind = FlipFlop (not is_on) }, send_all (not is_on))
    Conjunction inputs ->
      let inputs' = M.insert from pulse inputs
          m' = m { kind = Conjunction inputs' }
       in (m', send_all (any not inputs')) -- TODO: find all of the inputs of the conjunction
  where
    send_all p = map (\m_name -> Message name m_name p) dests

handleAllMessages :: State Pulses ()
handleAllMessages = do
  msg <- getMessage
  traceM $ show msg
  case msg of
    Nothing -> return ()
    Just msg' -> do
      dst <- gets $ M.lookup (dest msg') . mods
      case dst of
        Nothing -> return ()
        Just m -> do
          let (m', msgs) = runModule m msg'
          mapM_ appendMessage msgs
          modify $ \p -> p { mods = M.insert (dest msg') m' (mods p) }
      handleAllMessages

pressButton :: State Pulses ()
pressButton = do
  appendMessage $ Message "button" "broadcaster" False
  handleAllMessages

instance Part1 Day20 Modules where
  parse1 _ = runParser parseAllModules
  solve1 _ = show . counts . execState (replicateM_ 1000 pressButton) . Pulses mempty mempty

instance Part2 Day20 () where
  parse2 _ = const ()
  solve2 _ = show . const ()

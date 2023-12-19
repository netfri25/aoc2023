{-# OPTIONS_GHC -Wall -Wextra #-}
module Day19.Solution (Day19(..)) where

import Parts
import Parser

import Data.Char (isAlpha)
import Control.Applicative ((<|>))
import Data.Array
import qualified Data.Map as M

data Day19 = Day19 deriving Show

data Workflow = Workflow
  { name :: WorkflowName
  , rules :: [Rule]
  } deriving Show

parseWorkflow :: Parser String Workflow
parseWorkflow = Workflow <$> parseWorkflowName <* eqP '{' <*> sepBy (eqP ',') parseRule <* eqP '}'

type WorkflowName = String

parseWorkflowName :: Parser String WorkflowName
parseWorkflowName = spanP isAlpha

data Rule
  = Goto WorkflowName
  | Rule
    { variable :: Variable
    , ordering :: Ordering
    , value :: Int
    , goto :: WorkflowName
    } deriving Show

parseRule :: Parser String Rule
parseRule = rule <|> goto_rule
  where
    rule = Rule <$> parseVariable <*> parseOrdering <*> numP <* eqP ':' <*> parseWorkflowName
    goto_rule = Goto <$> parseWorkflowName

parseOrdering :: Parser String Ordering
parseOrdering = nextP >>= lift . flip lookup [('<', LT), ('>', GT)]

data Variable = X | M | A | S
  deriving (Show, Eq, Ord, Ix, Bounded)

parseVariable :: Parser String Variable
parseVariable = nextP >>= lift . flip lookup [('x', X), ('m', M), ('a', A), ('s', S)]

type Vars = Array Variable Int

parseVars :: Parser String Vars
parseVars = array (minBound, maxBound) <$> (eqP '{' *> sepBy (eqP ',') var_assign) <* eqP '}'
  where var_assign = (,) <$> parseVariable <* eqP '=' <*> numP

parseAllWorkflows :: Parser String (M.Map WorkflowName Workflow)
parseAllWorkflows = M.fromList <$> sepBy (eqP '\n') ((\w -> (name w, w)) <$> parseWorkflow)

instance Part1 Day19 (M.Map WorkflowName Workflow, [Vars]) where
  parse1 _ = runParser $ (,) <$> parseAllWorkflows <* ws <*> sepBy (eqP '\n') parseVars
  solve1 _ (workflows, vars) = show $ sum $ concatMap elems $ filter (isAccepted workflows "in") vars

nextWorkflow :: Vars -> [Rule] -> WorkflowName
nextWorkflow _ [] = undefined
nextWorkflow _ (Goto workflow:_) = workflow
nextWorkflow vars (Rule {variable, ordering, value, goto}:rest)
  | compare (vars ! variable) value == ordering = goto
  | otherwise = nextWorkflow vars rest

isAccepted :: M.Map WorkflowName Workflow -> WorkflowName -> Vars -> Bool
isAccepted _ "A" _ = True
isAccepted _ "R" _ = False
isAccepted workflows workflow vars = isAccepted workflows next_workflow vars
  where
    workflow_rules = rules $ workflows M.! workflow
    next_workflow = nextWorkflow vars workflow_rules

data Range = Range
  { rangeLow  :: Int
  , rangeHigh :: Int
  } deriving Show

rangeLength :: Range -> Int
rangeLength (Range start end) = end - start + 1

type VarsRanges = Array Variable Range

defaultRanges :: VarsRanges
defaultRanges = listArray (minBound, maxBound) (repeat $ Range 1 4000)

rulesCombs :: M.Map WorkflowName Workflow -> VarsRanges -> [Rule] -> Int
rulesCombs _ _ [] = undefined
rulesCombs workflows ranges (Goto workflow:_) = workflowCombs workflows ranges workflow
rulesCombs workflows ranges (Rule {variable, ordering, value, goto}:rest)
  | any ((<=0) . rangeLength) (elems ranges) = 0
  | otherwise = workflowCombs workflows accept_ranges goto + rulesCombs workflows deny_ranges rest
  where
    Range l h = ranges ! variable
    accept_range =
      case ordering of
        LT -> Range l (min h $ pred value)
        GT -> Range (max l $ succ value) h
        _ -> undefined
    deny_range =
      case ordering of
        LT -> Range (max l value) h
        GT -> Range l (min h value)
        _ -> undefined
    accept_ranges = ranges // [(variable, accept_range)]
    deny_ranges = ranges // [(variable, deny_range)]

workflowCombs :: M.Map WorkflowName Workflow -> VarsRanges -> WorkflowName -> Int
workflowCombs _ ranges "A" = varsCombs ranges
workflowCombs _ _ "R" = 0
workflowCombs workflows ranges name = rulesCombs workflows ranges $ rules $ workflows M.! name

varsCombs :: VarsRanges -> Int
varsCombs = product . map (fromIntegral . rangeLength) . elems

instance Part2 Day19 (M.Map WorkflowName Workflow) where
  parse2 _ = runParser parseAllWorkflows
  solve2 _ workflows = show $ workflowCombs workflows defaultRanges "in"

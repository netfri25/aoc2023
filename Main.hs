{-# OPTIONS_GHC -Wall -Wextra #-}
module Main (main, mainAll) where

import Parts
import Day1.Solution  (Day1(..))
import Day2.Solution  (Day2(..))
import Day3.Solution  (Day3(..))
import Day4.Solution  (Day4(..))
import Day5.Solution  (Day5(..))
import Day6.Solution  (Day6(..))
import Day7.Solution  (Day7(..))
import Day8.Solution  (Day8(..))
import Day9.Solution  (Day9(..))
import Day10.Solution (Day10(..))
import Day11.Solution (Day11(..))

import System.Directory (listDirectory)
import Data.List (isSuffixOf, sort, intercalate)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq, NFData)
import Text.Printf (printf)

type DayConstraint day input1 input2 = (Show day, Part1 day input1, Part2 day input2)

data Day = forall day i1 i2. DayConstraint day i1 i2 => Day day
deriving instance Show Day

days :: [Day]
days =
  [ Day Day1
  , Day Day2
  , Day Day3
  , Day Day4
  , Day Day5
  , Day Day6
  , Day Day7
  , Day Day8
  , Day Day9
  , Day Day10
  , Day Day11
  ]

main :: IO ()
main = runDay (last days)

printOutputs :: [Output] -> IO ()
printOutputs = putStrLn . intercalate "\n\n" . map show

runDay :: Day -> IO ()
runDay (Day day) = dayPaths day >>= traverse (runDayWith day) >>= printOutputs

mainAll :: IO ()
mainAll = mapM_ (\day -> runDay day >> putStrLn "") days

newtype TimeMS = TimeMS Float deriving Num
type Timed a = (TimeMS, a)

instance Show TimeMS where
  show (TimeMS t) = printf "%.3fms" t

data Output = Output FilePath (Timed String) (Timed String)

timeIO :: NFData a => IO a -> IO (Timed a)
timeIO m = do
  start <- getCPUTime
  x <- m
  finish <- x `deepseq` getCPUTime
  let elapsed_ms = fromIntegral (finish - start) / 1e9
  return (TimeMS elapsed_ms, x)

instance Show Output where
  show (Output path (t1, r1) (t2, r2)) = init $ unlines
    [ path ++ ":"
    , "Part 1: " ++ r1 ++ " (" ++ show t1 ++ ")"
    , "Part 2: " ++ r2 ++ " (" ++ show t2 ++ ")"
    , "Total time: " ++ show (t1 + t2)
    ]

dayPaths :: DayConstraint day i1 i2 => day -> IO [FilePath]
dayPaths day = do
  let dir = show day
  paths <- listDirectory dir
  return $ sort $ map (\p -> dir ++ "/" ++ p) $ filter (isSuffixOf ".txt") paths

runDayWith :: DayConstraint day i1 i2 => day -> FilePath -> IO Output
runDayWith day path = uncurry (Output path) <$> executeDayWith day path

executeDayWith :: DayConstraint day i1 i2 => day -> FilePath -> IO (Timed String, Timed String)
executeDayWith day path = do
  res1 <- timeIO (run1 day path)
  res2 <- timeIO (run2 day path)
  return (res1, res2)

runWithPart :: DayConstraint day i1 i2 => (day -> String -> String) -> day -> FilePath -> IO String
runWithPart part day path = part day <$> readFile path

run1 :: DayConstraint day i1 i2 => day -> FilePath -> IO String
run1 = runWithPart part1

run2 :: DayConstraint day i1 i2 => day -> FilePath -> IO String
run2 = runWithPart part2

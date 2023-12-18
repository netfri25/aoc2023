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
import Day12.Solution (Day12(..))
import Day13.Solution (Day13(..))
import Day14.Solution (Day14(..))
import Day15.Solution (Day15(..))
import Day16.Solution (Day16(..))
import Day17.Solution (Day17(..))
import Day18.Solution (Day18(..))

import System.Directory (listDirectory)
import Data.List (isSuffixOf, sort)
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
  , Day Day12
  , Day Day13
  , Day Day14
  , Day Day15
  , Day Day16
  , Day Day17
  , Day Day18
  ]

main :: IO ()
main = runDay (last days)

runDay :: Day -> IO ()
runDay (Day day) = dayPaths day >>= mapM_ (runDayWith day)

mainAll :: IO ()
mainAll = mapM_ (\day -> runDay day >> putStrLn "") days

newtype TimeMS = TimeMS Float deriving Num
type Timed a = (TimeMS, a)

instance Show TimeMS where
  show (TimeMS t) = printf "%.3fms" t

timeIO :: NFData a => IO a -> IO (Timed a)
timeIO m = do
  start <- getCPUTime
  x <- m
  finish <- x `deepseq` getCPUTime
  let elapsed_ms = fromIntegral (finish - start) / 1e9
  return (TimeMS elapsed_ms, x)

dayPaths :: DayConstraint day i1 i2 => day -> IO [FilePath]
dayPaths day = do
  let dir = show day
  paths <- listDirectory dir
  return $ sort $ map (\p -> dir ++ "/" ++ p) $ filter (isSuffixOf ".txt") paths

runDayWith :: DayConstraint day i1 i2 => day -> FilePath -> IO ()
runDayWith day path = do
  (t1, r1) <- timeIO $ run1 day path
  putStrLn $ path ++ ":"
  putStrLn $ "Part 1: " ++ r1 ++ " (" ++ show t1 ++ ")"
  (t2, r2) <- timeIO $ run2 day path
  putStrLn $ "Part 2: " ++ r2 ++ " (" ++ show t2 ++ ")"
  putStrLn $ "Total time: " ++ show (t1 + t2)
  putStrLn ""

runWithPart :: DayConstraint day i1 i2 => (day -> String -> String) -> day -> FilePath -> IO String
runWithPart part day path = part day <$> readFile path

run1 :: DayConstraint day i1 i2 => day -> FilePath -> IO String
run1 = runWithPart part1

run2 :: DayConstraint day i1 i2 => day -> FilePath -> IO String
run2 = runWithPart part2

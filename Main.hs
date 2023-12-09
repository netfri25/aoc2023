{-# OPTIONS_GHC -Wall -Wextra #-}
module Main (main, mainAll) where

import Parts
import Day1.Solution (Day1(..))
import Day2.Solution (Day2(..))
import Day3.Solution (Day3(..))
import Day4.Solution (Day4(..))
import Day5.Solution (Day5(..))
import Day6.Solution (Day6(..))
import Day7.Solution (Day7(..))
import Day8.Solution (Day8(..))
import Day9.Solution (Day9(..))

import System.Directory (listDirectory)
import Data.List (isSuffixOf, sort, intercalate)

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
  ]

main :: IO ()
main = runDay (last days)

printOutputs :: [Output] -> IO ()
printOutputs = putStrLn . intercalate "\n\n" . map show

runDay :: Day -> IO ()
runDay (Day day) = dayPaths day >>= traverse (runDayWith day) >>= printOutputs

mainAll :: IO ()
mainAll = mapM_ (\day -> runDay day >> putStrLn "") days

data Output = Output FilePath Result Result

instance Show Output where
  show (Output path r1 r2) = init $ unlines [path ++ ":", "Part 1: " ++ show r1, "Part 2: " ++ show r2]

dayPaths :: DayConstraint day i1 i2 => day -> IO [FilePath]
dayPaths day = do
  let dir = show day
  paths <- listDirectory dir
  return $ sort $ map (\p -> dir ++ "/" ++ p) $ filter (isSuffixOf ".txt") paths

runDayWith :: DayConstraint day i1 i2 => day -> FilePath -> IO Output
runDayWith day path = uncurry (Output path) <$> executeDayWith day path

executeDayWith :: DayConstraint day i1 i2 => day -> FilePath -> IO (Result, Result)
executeDayWith day path = do
  res1 <- run1 day path
  res2 <- run2 day path
  return (res1, res2)

run1 :: DayConstraint day i1 i2 => day -> String -> IO Result
run1 day path = part1 day <$> readFile path

run2 :: DayConstraint day i1 i2 => day -> String -> IO Result
run2 day path = part2 day <$> readFile path

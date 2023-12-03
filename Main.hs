module Main (main, mainAll) where

import Parts
import Day1.Solution (Day1(..))
import Day2.Solution (Day2(..))
import Day3.Solution (Day3(..))

type DayConstraint a = (InputPath a, Show a, Part2 a)

data Day = forall a. DayConstraint a => Day a

deriving instance Show Day

days :: [Day]
days =
  [ Day Day1
  , Day Day2
  , Day Day3
  ]

main :: IO ()
main = do
  let day = last days
  print day

  putStrLn "\nRegular Input:"
  runDayWith inputPath day

  putStrLn "\nExample Input:"
  runDayWith examplePath day

mainAll :: IO ()
mainAll = mapM_ (runDayWith inputPath) days

runDayWith :: (forall d. DayConstraint d => d -> FilePath) -> Day -> IO ()
runDayWith get_path (Day day) = do
  res1 <- run1 (Day day) (get_path day)
  putStrLn $ unwords ["Part 1:", show res1]
  res2 <- run2 (Day day) (get_path day)
  putStrLn $ unwords ["Part 2:", show res2]

run1 :: Day -> String -> IO Result
run1 (Day day) path = part1 day <$> readFile path

run2 :: Day -> String -> IO Result
run2 (Day day) path = part2 day <$> readFile path

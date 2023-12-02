module Main (main) where

import Parts
import Day1.Solution (Day1(..))
import Day2.Solution (Day2(..))

type DayConstraint a = (InputName a, Show a, Part2 a)

data Day = forall a. DayConstraint a => Day a

deriving instance Show Day

days :: [Day]
days =
  [ Day Day1
  , Day Day2
  ]

main :: IO ()
main = do
  let day = last days
  print day

  putStrLn "\nExample Input:"
  runDayWith exampleName day

  putStrLn "\nRegular Input:"
  runDayWith inputName day

mainAll :: IO ()
mainAll = mapM_ (runDayWith inputName) days

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

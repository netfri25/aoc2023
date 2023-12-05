import System.Environment (getArgs)
import Control.Monad (when)
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import System.Directory (doesDirectoryExist, createDirectory)
import Data.Maybe (maybe)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (putStrLn usage >> exitFailure)
  let maybe_day = readMaybe (head args)
  day <- maybe (panic "Enter day as a number") return maybe_day
  createDay day

usage :: String
usage = "Usage: runghc create <day number>"

panic :: String -> IO a
panic s = putStrLn ("[ERROR] " ++ s) >> exitFailure

createDay :: Int -> IO ()
createDay day = do
  let dir_path = "./Day" ++ show day
  exists <- doesDirectoryExist dir_path
  when exists $ panic $ unwords ["day", show day, "directory already exists:", show (drop 2 dir_path)]
  createDirectory dir_path
  let solution_path = dir_path ++ "/Solution.hs"
  let example_path = dir_path ++ "/example.txt"
  let input_path = dir_path ++ "/input.txt"
  writeFile solution_path $ solutionContent day
  writeFile example_path ""
  writeFile input_path ""

solutionContent :: Int -> String
solutionContent day_n = unlines
  [ "{-# OPTIONS_GHC -Wall -Wextra #-}"
  , "module " ++ day ++ ".Solution (" ++ day ++ "(..)) where"
  , ""
  , "import Parts"
  , ""
  , "data " ++ day ++ " = " ++ day ++ " deriving Show"
  , ""
  , "instance InputPath " ++ day ++ " where"
  , "  examplePath = const \"" ++ day ++ "/example.txt\""
  , "  inputPath = const \"" ++ day ++ "/input.txt\""
  , ""
  , "instance Part1 " ++ day ++ " () where"
  , "  parse1 _ = undefined"
  , "  solve1 _ = Result . const Todo"
  , ""
  , "instance Part2 " ++ day ++ " () where"
  , "  parse2 _ = undefined"
  , "  solve2 _ = Result . const Todo"
  ]
  where day = "Day" ++ show day_n

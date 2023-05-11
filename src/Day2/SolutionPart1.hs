module Day2.SolutionPart1 where

import           Data.Maybe (mapMaybe)

type Score = Int

data Shape = Rock | Paper | Scissors
  deriving (Eq, Show)

loadInput :: FilePath -> IO [(Shape, Shape)]
loadInput fp = do
  content <- readFile fp
  let inputs = mapMaybe (parseInput . words) $ lines content
  pure inputs
  where
    parseInput :: [String] -> Maybe (Shape, Shape)
    parseInput [x, y] =
      case (inputToShape x, inputToShape y) of
        (Just a, Just b) -> Just (a, b)
        _                -> Nothing

    inputToShape :: String -> Maybe Shape
    inputToShape "X" = Just Rock
    inputToShape "Y" = Just Paper
    inputToShape "Z" = Just Scissors
    inputToShape "A" = Just Rock
    inputToShape "B" = Just Paper
    inputToShape "C" = Just Scissors
    inputToShape _   = Nothing

evaluateInput :: (Shape, Shape) -> Score
evaluateInput (Rock, Rock)         = 4
evaluateInput (Rock, Paper)        = 8
evaluateInput (Rock, Scissors)     = 3
evaluateInput (Paper, Rock)        = 1
evaluateInput (Paper, Paper)       = 5
evaluateInput (Paper, Scissors)    = 9
evaluateInput (Scissors, Rock)     = 7
evaluateInput (Scissors, Paper)    = 2
evaluateInput (Scissors, Scissors) = 6

runDay2Part1 :: IO ()
runDay2Part1 = do
  -- inputs <- loadInput "src/Day2/sampleInput.txt"
  inputs <- loadInput "src/Day2/input.txt"
  let scores = map evaluateInput inputs
  let totalScore = sum scores
  putStrLn $ "Total score: " ++ show totalScore

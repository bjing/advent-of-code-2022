module Day2.SolutionPart2 where

import           Data.Maybe (mapMaybe)

type Score = Int

data Outcome = Lose | Draw | Win
  deriving (Eq, Read, Show)

data Shape = Rock | Paper | Scissors
  deriving (Eq, Show)

loadInput :: FilePath -> IO [(Shape, Outcome)]
loadInput fp = do
  content <- readFile fp
  let inputs = mapMaybe (parseInput . words) $ lines content
  pure inputs
  where
    parseInput :: [String] -> Maybe (Shape, Outcome)
    parseInput [a, b] =
      case (parseShape a, parseOutcome b) of
        (Just x, Just y) -> Just (x, y)
        _                -> Nothing
    parseInput _ = Nothing

    parseShape :: String -> Maybe Shape
    parseShape "A" = Just Rock
    parseShape "B" = Just Paper
    parseShape "C" = Just Scissors
    parseShape _   = Nothing

    parseOutcome :: String -> Maybe Outcome
    parseOutcome "X" = Just Lose
    parseOutcome "Y" = Just Draw
    parseOutcome "Z" = Just Win
    parseOutcome _   = Nothing

evaluateInput :: (Shape, Outcome) -> Score
evaluateInput (Rock, Lose)     = 3
evaluateInput (Rock, Draw)     = 4
evaluateInput (Rock, Win)      = 8
evaluateInput (Paper, Lose)    = 1
evaluateInput (Paper, Draw)    = 5
evaluateInput (Paper, Win)     = 9
evaluateInput (Scissors, Lose) = 2
evaluateInput (Scissors, Draw) = 6
evaluateInput (Scissors, Win)  = 7

runDay2Part2 :: IO ()
runDay2Part2 = do
  -- inputs <- loadInput "src/Day2/sampleInput.txt"
  inputs <- loadInput "src/Day2/input.txt"
  let scores = map evaluateInput inputs
  let totalScore = sum scores
  putStrLn $ "Total score: " ++ show totalScore

module Day2.SolutionPart2 where

type Score = Int

data Outcome = Lose | Draw | Win
  deriving (Eq, Read, Show)

data Shape = Rock | Paper | Scissors
  deriving (Eq, Show)

loadInput :: FilePath -> IO [(Shape, Outcome)]
loadInput fp = do
  content <- readFile fp
  let inputs = map (parseTuple . listToTuple . words) $ lines content
  pure inputs
  where
    mapTuple :: (a -> b) -> (a, a) -> (b, b)
    mapTuple f (a1, a2) = (f a1, f a2)

    listToTuple :: [String] -> (String, String)
    listToTuple [x, y] = (x, y)
    listToTuple _      = error "List needs to have exactly two elments"

    parseTuple :: (String, String) -> (Shape, Outcome)
    parseTuple (a, b) = (parseShape a, parseOutcome b)

    parseShape :: String -> Shape
    parseShape "A" = Rock
    parseShape "B" = Paper
    parseShape "C" = Scissors

    parseOutcome :: String -> Outcome
    parseOutcome "X" = Lose
    parseOutcome "Y" = Draw
    parseOutcome "Z" = Win

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

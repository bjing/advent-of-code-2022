module Day2.Solution where

type Score = Int

data Input = X | Y | Z | A | B | C
  deriving (Eq, Read, Show)

data Shape = Rock | Paper | Scissors
  deriving (Eq, Show)

loadInput :: FilePath -> IO [(Shape, Shape)]
loadInput fp = do
  content <- readFile fp
  let inputs = map (mapTuple inputToShape . listToTuple . words) $ lines content
  pure inputs
  where
    mapTuple :: (a -> b) -> (a, a) -> (b, b)
    mapTuple f (a1, a2) = (f a1, f a2)

    listToTuple :: [String] -> (String, String)
    listToTuple [x, y] = (x, y)
    listToTuple _      = error "List needs to have exactly two elments"

    inputToShape :: String -> Shape
    inputToShape "X" = Rock
    inputToShape "Y" = Paper
    inputToShape "Z" = Scissors
    inputToShape "A" = Rock
    inputToShape "B" = Paper
    inputToShape "C" = Scissors

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

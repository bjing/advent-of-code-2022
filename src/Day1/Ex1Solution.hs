module Day1.Ex1Solution where

import           Data.List.Split (splitOn)

loadCaloriesFromFile :: FilePath -> IO [[Int]]
loadCaloriesFromFile fp = do
  content <- readFile fp
  let linesOfContent = splitOn [""] $ lines content
  let result = map (map (\x -> read x :: Int)) linesOfContent
  pure result

runDay1Ex1 :: IO ()
runDay1Ex1 = do
  -- caloriesByPerson <- loadCaloriesFromFile "src/Day1/ex1SampleInput.txt"
  caloriesByPerson <- loadCaloriesFromFile "src/Day1/ex1Input.txt"
  let res = maximum $ map sum caloriesByPerson
  print $ "Highest calories carried: " ++ show res


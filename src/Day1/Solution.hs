module Day1.Solution where

import           Data.List       (sortBy)
import           Data.List.Split (splitOn)

loadCaloriesFromFile :: FilePath -> IO [[Int]]
loadCaloriesFromFile fp = do
  content <- readFile fp
  let linesOfContent = splitOn [""] $ lines content
  let result = map (map (\x -> read x :: Int)) linesOfContent
  pure result

runDay1Part1 :: IO ()
runDay1Part1 = do
  -- caloriesByPerson <- loadCaloriesFromFile "src/Day1/sampleInput.txt"
  caloriesByPerson <- loadCaloriesFromFile "src/Day1/input.txt"
  let res = maximum $ map sum caloriesByPerson
  print $ "Highest calories carried: " ++ show res

runDay1Part2 :: IO ()
runDay1Part2 = do
  -- caloriesByPerson <- loadCaloriesFromFile "src/Day1/sampleInput.txt"
  caloriesByPerson <- loadCaloriesFromFile "src/Day1/input.txt"
  let calorieByPerson = map sum caloriesByPerson
  let res = sum $ take 3 $ sortBy (\a b -> compare b a) calorieByPerson
  print $ "Total of calories carried by top 3 elves: " ++ show res


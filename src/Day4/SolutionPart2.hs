module Day4.SolutionPart2 where

import qualified Data.List.Split as S
import qualified Data.List as L

runDay4Part2 :: IO ()
runDay4Part2 = do
  input <- loadInput "src/Day4/input.txt"
  let pairs = map extractPairsFromInputLine input
  let overlaps = map overlap pairs
  let numOverlaps = length $ filter (not . null) overlaps
  putStrLn $ "Number of overlaps: " ++ show numOverlaps

loadInput :: FilePath -> IO [String]
loadInput fp = do
  content <- readFile fp
  pure $ lines content

genList :: [Int] -> ([Int], [Int])
genList (a:b:x:y:xs) = ([a..b], [x..y])

extractPairsFromInputLine :: String -> ([Int], [Int])
extractPairsFromInputLine = genList . map stringToInt . S.splitOneOf "-," 
  where 
    stringToInt :: String -> Int
    stringToInt ss = do 
      read ss :: Int
  
overlap :: ([Int], [Int]) -> [Int]
overlap (first, second) = first `L.intersect` second

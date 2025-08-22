module Day4.SolutionPart1 where

import qualified Data.List.Split as S
import qualified Data.List as L

runDay4Part1 :: IO ()
runDay4Part1 = do
  input <- loadInput "src/Day4/input.txt"
  let pairs = map extractPairsFromInputLine input
  let containedPairs = map contained pairs
  let numPairsContained = length $ filter id containedPairs
  putStrLn $ "Number of pairs containing each other: " ++ show numPairsContained

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
  
contained :: ([Int], [Int]) -> Bool
contained (first, second) = first `L.isInfixOf` second || second `L.isInfixOf` first

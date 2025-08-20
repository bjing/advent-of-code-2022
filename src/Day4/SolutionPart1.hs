module Day4.SolutionPart1 where

import qualified Data.List.Split as S
import qualified Data.List as L

runDay4Part1 :: IO ()
runDay4Part1 = do
  input <- loadInput "src/Day4/input.txt"
  let pairs = map extractPairsFromInputLine input
  let overlaps = map overlap pairs
  let numOverlaps = length $ filter id overlaps
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
  
overlap :: ([Int], [Int]) -> Bool
overlap (first, second) = first `L.isInfixOf` second || second `L.isInfixOf` first

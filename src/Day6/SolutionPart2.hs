module Day6.SolutionPart2 where

import qualified Data.Set as Set

runDay6Part2 :: IO ()
runDay6Part2 = do
    content <- loadInput "src/Day6/input.txt"
    let result = process content
    print result

loadInput :: FilePath -> IO String
loadInput fp = do
    readFile fp

process :: String -> Int
process = process' 0

process' :: Int -> String -> Int
process' charsProcessed s = 
    if allUnique $ take 14 s
    then
        charsProcessed + 14
    else
        process' (charsProcessed + 1) (tail s)

allUnique :: String -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)

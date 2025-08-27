module Day6.SolutionPart1 where

import qualified Data.Set as Set

runDay6Part1 :: IO ()
runDay6Part1 = do
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
    if allUnique $ take 4 s
    then
        charsProcessed + 4
    else
        process' (charsProcessed + 1) (tail s)

allUnique :: String -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)

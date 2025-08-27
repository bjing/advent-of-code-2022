{-# LANGUAGE OverloadedStrings #-}

module Day5.SolutionPart1 where

import qualified Data.Map as M
import Data.Map (Map) 
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as T

type Stack = String
type Stacks = Map Int Stack

data Move = Move {
    cratesToMove :: Int,
    stackFrom :: Int, 
    stackTo :: Int
} deriving Show

runDay5Part1 :: IO ()
runDay5Part1 = do
    inputLines <- loadInput "src/Day5/input.txt"
    let moves = parseInput inputLines
    let finalStack = foldl rearrange initStacks moves
    let result = head <$> M.elems finalStack
    print result

loadInput :: FilePath -> IO [String]
loadInput fp = do
    content <- readFile fp
    pure $ lines content

parseInput :: [String] -> [Move]
parseInput lines = parseMove <$> filteredLines
    where
        filteredLines = filter (List.isPrefixOf "move") lines

parseMove :: String -> Move
parseMove s =
  case words s of
    ["move", x, "from", y, "to", z] -> 
        Move {
            cratesToMove = read x, 
            stackFrom = read y, 
            stackTo = read z
        }
    _ -> error "Invalid format"


initStacks :: Stacks
initStacks = M.fromList [
    (1, "CFBLDPZS"),
    (2, "BWHPGVN"),
    (3, "GJBWF"),
    (4, "SCWLFNJG"),
    (5, "HSMPTLJW"),
    (6, "SFGWCB"),
    (7, "WBQMPTH"),
    (8, "TWSF"),
    (9, "RCN")]

rearrange :: Stacks -> Move -> Stacks
rearrange stacks move = 
    let from = stackFrom move
        to = stackTo move
        fromStack = M.lookup from stacks
        toStack = M.lookup to stacks
        (newFrom, newTo) = Maybe.fromJust $ moveStack (cratesToMove move) <$> fromStack <*> toStack 
    in
        updateStacks stacks [(from, newFrom), (to, newTo)]
        
moveStack :: Int -> Stack -> Stack -> (Stack, Stack)
moveStack 0 fromStack toStack = (fromStack, toStack)
moveStack _ [] toStack = ([], toStack)
moveStack movesLeft fromStack@(hf:fs) toStack = 
    moveStack (movesLeft - 1) fs (hf:toStack)

updateStacks :: Stacks -> [(Int, String)] -> Stacks
updateStacks stacks [] = stacks
updateStacks stacks ((index, stack):updates) = 
    updateStacks updatedStacks updates
    where 
        updatedStacks = M.insert index stack stacks
    
processInputLine :: String -> String
processInputLine = T.unpack . T.replace "to" "" . T.replace "from" "" . T.replace "move" "" . T.pack

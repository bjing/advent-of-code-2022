{-# LANGUAGE OverloadedStrings #-}

module Day5.SolutionPart1 where
    
-- TODO Refactor this module to handle state better

import qualified Data.Map as M
import Data.Map (Map) 
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as T
import Control.Monad.State.Lazy

type Stack = String
type StackMap = Map Int Stack
type Stacks = State StackMap ()
data Move = Move {
    cratesToMove :: Int,
    stackFrom :: Int, 
    stackTo :: Int
} deriving Show

runDay5Part1 :: IO ()
runDay5Part1 = do
    inputLines <- loadInput "src/Day5/input.txt"
    let moves = parseInput inputLines
    let stacks = mapM_ rearrange moves
    let finalStack = execState stacks initStacks
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

initStacks :: StackMap
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

rearrange :: Move -> Stacks
rearrange move = do
    stacks <- get
    let from = stackFrom move
    let to = stackTo move
    let fromStack = M.lookup from stacks
    let toStack = M.lookup to stacks
    let (newFrom, newTo) = Maybe.fromJust $ moveStack (cratesToMove move) <$> fromStack <*> toStack 
    updateStacks [(from, newFrom), (to, newTo)]

updateStacks :: [(Int, String)] -> Stacks
updateStacks [] = do
    pure ()
updateStacks ((index, stack):updates) = do
    stacks <- get
    put $ M.insert index stack stacks
    updateStacks updates

moveStack :: Int -> Stack -> Stack -> (Stack, Stack)
moveStack 0 fromStack toStack = (fromStack, toStack)
moveStack _ [] toStack = ([], toStack)
moveStack movesLeft fromStack@(hf:fs) toStack = 
    moveStack (movesLeft - 1) fs (hf:toStack)
   
processInputLine :: String -> String
processInputLine = T.unpack . T.replace "to" "" . T.replace "from" "" . T.replace "move" "" . T.pack

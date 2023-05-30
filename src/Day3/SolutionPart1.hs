module Day3.SolutionPart1 where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Maybe
import qualified Data.Set   as S

type Rucksack = String

type Item = Char

type Priority = Int

getCommonItemFromRuckSack :: Rucksack -> Item
getCommonItemFromRuckSack s =
  S.elemAt 0 $ S.fromList first `S.intersection` S.fromList second
  where
    (first, second) = L.splitAt (length s `div` 2) s

loadInput :: FilePath -> IO [Rucksack]
loadInput fp = do
  content <- readFile fp
  pure $ lines content

priorityMapping :: M.Map Item Priority
priorityMapping = M.fromList $ zip ['a' .. 'z'] [1 ..] ++ zip ['A' .. 'Z'] [27 ..]

convertItemsToPriorities :: [Item] -> [Priority]
convertItemsToPriorities = Maybe.mapMaybe (`M.lookup` priorityMapping)

runDay3Part1 :: IO ()
runDay3Part1 = do
  rucksacks <- loadInput "src/Day3/input.txt"
  let commonItems = map getCommonItemFromRuckSack rucksacks
  let priorities = convertItemsToPriorities commonItems
  print $ sum priorities

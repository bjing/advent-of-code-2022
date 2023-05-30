module Day3.SolutionPart2 where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Maybe
import qualified Data.Set   as S

type Rucksack = String

type Item = Char

getBadgeFromGroup :: [Rucksack] -> Item
getBadgeFromGroup sacks =
  head $ foldr L.intersect (concat sacks) sacks

loadInput :: FilePath -> IO [[Rucksack]]
loadInput fp = do
  content <- readFile fp
  let rucksacks = lines content
  pure $ groupRucksacks rucksacks [[]]
  where
    groupRucksacks :: [Rucksack] -> [[Rucksack]] -> [[Rucksack]]
    groupRucksacks (x : y : z : rest) res = groupRucksacks rest ([x, y, z] : res)
    groupRucksacks [] res = res

priorityMapping :: M.Map Char Int
priorityMapping = M.fromList $ zip ['a' .. 'z'] [1 ..] ++ zip ['A' .. 'Z'] [27 ..]

runDay3Part2 :: IO ()
runDay3Part2 = do
  rucksacks <- loadInput "src/Day3/input.txt"
  let commonItemsForSacks = map getBadgeFromGroup (filter (/= []) rucksacks)
  let priorities = Maybe.mapMaybe (`M.lookup` priorityMapping) commonItemsForSacks
  print $ sum priorities

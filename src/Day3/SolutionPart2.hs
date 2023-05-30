module Day3.SolutionPart2 where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Maybe
import qualified Data.Set   as S

type Rucksack = String

type Badge = Char

type Priority = Int

getBadgeFromGroup :: [Rucksack] -> Badge
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

priorityMapping :: M.Map Badge Priority
priorityMapping = M.fromList $ zip ['a' .. 'z'] [1 ..] ++ zip ['A' .. 'Z'] [27 ..]

convertBadgesToPriorities :: [Badge] -> [Priority]
convertBadgesToPriorities = Maybe.mapMaybe (`M.lookup` priorityMapping)

runDay3Part2 :: IO ()
runDay3Part2 = do
  rucksacks <- loadInput "src/Day3/input.txt"
  let badges = map getBadgeFromGroup (filter (/= []) rucksacks)
  let priorities = convertBadgesToPriorities badges
  print $ sum priorities

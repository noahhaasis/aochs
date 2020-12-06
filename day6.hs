module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Advent

type Group = [Set Char]

readGroups :: FilePath -> IO [Group]
readGroups = fmap (map parseGroup . flip split [] . lines) . readFile
  where parseGroup = map Set.fromList

part1 :: [Group] -> Int
part1 = sum . map (Set.size . Set.unions)

part2 :: [Group] -> Int
part2 = sum . map (Set.size . intersections)

intersections :: Ord a => [Set a] -> Set a
intersections [] = Set.empty
intersections [a] = a
intersections (s : r) = Set.intersection s $ intersections r

main :: IO ()
main = readGroups "input6" >>= (putStrLn . show . part2)

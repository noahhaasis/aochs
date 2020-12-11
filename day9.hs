module Main where

import Control.Monad (join)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set

group :: Int -> [Int] -> [[Int]]
group n xs | length xs < n = []
group n xs = take n xs : group n (tail xs)

part1 :: [Int] -> Maybe Int
part1 l = fst <$> find (not . isValid) numPairs
  where
    numPairs = zip (drop windowSize l) (possibleSums <$> group windowSize l)
    windowSize = 25
    possibleSums xs = Set.fromList [x + y | x <- xs, y <- xs]
    isValid = uncurry Set.member

findContigiousSet :: Int -> [Int] -> Maybe [Int]
findContigiousSet n l = join $ find isJust (tryFromStart [] 0 <$> allLists)
  where
    tryFromStart _ accSum _ | accSum > n = Nothing
    tryFromStart accList accSum _ | accSum == n = Just accList
    tryFromStart accList accSum (x:xs) = tryFromStart (x:accList) (accSum + x) xs
    allLists = [drop n l | n <- [0..length l - 2]]
    isJust (Just _) = True
    isJust Nothing = False

part2 :: [Int] -> Maybe Int
part2 l = (\l -> maximum l + minimum l) <$> findContigiousSet solution1 l
  where solution1 = 542529149

main :: IO ()
main = getInput "input9" >>= (print . part2)
  where getInput = fmap (map read . words) . readFile

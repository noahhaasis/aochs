module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Advent

nextAdapter :: Int -> Set Int -> Int
nextAdapter n = Set.findMin . Set.filter (\r -> (r - n)  > 0 && (r - n) <= 3)

part1 :: [Int] -> Int
part1 l = count (==1) diffList * (1 + count (==3) diffList)
  where ratings = Set.fromList l
        diffList = go [] 0 ratings
        go acc _ s | Set.null s = acc
        go acc n s = go (diff : acc) next newSet
          where next = nextAdapter n s
                newSet = Set.delete next s
                diff = next - n

deviceJolts :: [Int] -> Int
deviceJolts = (+3) . maximum

getInput :: FilePath -> IO [Int]
getInput = fmap (map read . words) . readFile

main :: IO ()
main = getInput "input10" >>= (print . part1)

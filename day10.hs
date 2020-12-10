module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

import Advent

plugsInto :: Int -> Int -> Bool
plugsInto n r = (r - n)  > 0 && (r - n) <= 3

nextAdapter :: Int -> Set Int -> Int
nextAdapter n = Set.findMin . Set.filter (plugsInto n)

part1 :: [Int] -> Int
part1 l = count (==1) diffList * (1 + count (==3) diffList)
  where ratings = Set.fromList l
        diffList = go [] 0 ratings
        go acc _ s | Set.null s = acc
        go acc n s = go (diff : acc) next newSet
          where next = nextAdapter n s
                newSet = Set.delete next s
                diff = next - n

getInput :: FilePath -> IO [Int]
getInput = fmap (map read . words) . readFile

part2 :: [Int] -> Int
part2 l = evalState (paths graph 0) (C Map.empty)
  where graph = adaptersToGraph $ Set.insert 0 $ Set.fromList l

type Graph = Map Int [Int]

adaptersToGraph :: Set Int -> Graph
adaptersToGraph s = Map.fromList $ foldr (\n l -> (n, nextNodes n) : l) [] s
  where maxNode = Set.findMax s
        nextNodes n = Set.toList $ Set.filter (plugsInto n) s

-- Memoized function for performance

newtype SearchContext = C (Map Int Int)

paths :: Graph -> Int -> State SearchContext Int
paths graph start = do
  (C ctx) <- get
  case Map.lookup start ctx of
    Just n -> return n
    Nothing ->
      case Map.lookup start graph of
        -- There's only one top node and it's the last adapter
        Just [] -> memoized start $ 1
        Just nexts -> do
          res <- mapM (paths graph) nexts
          memoized start $ sum res
        Nothing -> memoized start $ 0

memoized :: Int -> Int -> State SearchContext Int
memoized node value = do
  modify (\(C ctx) -> C (Map.insert node value ctx))
  return value

main :: IO ()
main = getInput "input10" >>= (print . part2)

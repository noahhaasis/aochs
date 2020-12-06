module Advent (split, count) where

split :: Eq a => [a] -> a -> [[a]]
split s c =
  case splitOne s c of
    ([], []) -> []
    (l, []) -> [l]
    ([], r) -> split r c
    (l, r) -> l : split r c
  where
    splitOne = go []
    go acc (x : xs) c | x == c = (acc, xs)
    go acc (x : xs) c = go (acc ++ [x]) xs c
    go acc [] _ = (acc, [])


count :: (a -> Bool) -> [a] -> Int
count f l = length $ filter f l

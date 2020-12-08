module Advent (split, count, contains) where

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

splitWith :: Eq a => [a] -> [a] -> [[a]]
splitWith s p =
  case splitOne s p of
    ([], []) -> []
    (l, []) -> [l]
    ([], r) -> splitWith r p
    (l, r) -> l : splitWith r p
  where
    splitOne = go []
    go acc a p | length a < length p = (a, [])
    go acc a p | take (length p) a == p = (acc, drop (length p) a)
    go acc (x : xs) p = go (acc ++ [x]) xs p
    go acc [] _ = (acc, [])

contains :: Eq a => [a] -> [a] -> Bool
contains a b | length a < length b = False
contains a b | take (length b) a == b = True
contains (_:xs) b = xs `contains` b

count :: (a -> Bool) -> [a] -> Int
count f l = length $ filter f l

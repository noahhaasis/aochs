solution1 :: [Int] -> Int
solution1 measurements = length $ filter (\(a, b) -> b > a) pairs
  where pairs = zip (take (length measurements - 1) measurements) (drop 1 measurements)

solution2 :: [Int] -> Int
solution2 measurements = solution1 slidingValues
  where
    slidingValues = zipWith (+) (zipWith (+) list1 list2) list3
    list1 = take (length measurements - 2) measurements
    list2 = drop 1 $ take (length measurements - 1) measurements
    list3 = drop 2 measurements

main :: IO ()
main = (solution2 . fmap read . words) <$> readFile "input" >>= print

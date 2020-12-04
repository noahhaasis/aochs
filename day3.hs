module Main where

data Square
  = Empty
  | Tree
  deriving Show

type Field = [[Square]]

part1 :: Field -> Int
part1 = treesHitForSlope 3 1

treesHitForSlope :: Int -> Int -> Field -> Int
treesHitForSlope right down field = sum ((\(i, j) -> countHit (field !! i !! j)) <$> coordinates)
  where
    coordinates =
      [ (i, (i `div` down)*right `mod` width)
      | i <- [0..length field - 1], i `mod` down == 0
      ]
    countHit Empty = 0
    countHit Tree = 1
    width = case field of
              [] -> 0
              (x : xs) -> length x

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

part2 :: Field -> Int
part2 f = product (($ f) <$>  (uncurry treesHitForSlope <$> slopes))

readField :: FilePath -> IO Field
readField = fmap (map parseLine . lines) . readFile

parseLine :: String -> [Square]
parseLine = map parseSquare
  where parseSquare '#' = Tree
        parseSquare '.' = Empty

main :: IO ()
main = readField "input3" >>= putStrLn . show . part2

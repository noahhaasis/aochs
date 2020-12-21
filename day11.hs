module Main where

import Advent

data Square
  = Empty
  | Taken
  | Floor
  deriving (Show, Eq)

type Grid = [[Square]]

squareAt :: Grid -> (Int, Int) -> Square
squareAt g (x, y)
  | x >= 0
  && y >= 0
  && x < length g
  && length g > 0
  && y < length (g!!0) = g !! x !! y
squareAt _ _ = Empty

takenNeighbours :: (Int, Int) -> Grid -> Int
takenNeighbours (x, y) g = count (==Taken) neighbours
  where
    neighbours = squareAt g . (\(a,b) -> (x+a,y+b)) <$> offsets
    offsets =
      [ (1, 1), (1, 0), (0, 1), (-1, 1)
      , (-1, -1), (-1, 0), (0, -1), (1, -1)
      ]

newState :: Square -> Int -> Square
newState Floor _ = Floor
newState Empty 0 = Taken
newState Taken n | n >= 4 = Empty
newState s _ = s

complexStep :: Grid -> Grid
complexStep g =
  [ [ newState (squareAt g (x,y)) (takenNeighbours (x,y) g)
    | y <- [0..width-1]
    ]
    | x <- [0..height-1]
  ]
  where height = length g
        width = if length g == 0 then 0 else length (g!!0)

countTakenSeats :: Grid -> Int
countTakenSeats = sum . map (count (==Taken))

getInput :: FilePath -> IO Grid
getInput = fmap (map (map parseSquare) . lines) . readFile
  where parseSquare 'L' = Empty
        parseSquare '#' = Taken
        parseSquare '.' = Floor

stabilize :: (Grid -> Grid) -> Grid -> Grid
stabilize step g = if g == next then g else stabilize step next
  where next = simpleStep g

part1 :: Grid -> Int
part1 = countTakenSeats . simpleStep

newState2 :: Square -> Int -> Square
newState2 Floor _ = Floor
newState2 Empty 0 = Taken
newState2 Taken n | n >= 5 = Empty
newState2 s _ = s

takenNeighbours2 :: (Int, Int) -> Grid -> Int
takenNeighbours2 = undefined

simpleStep :: Grid -> Grid
simpleStep g =
  [ [ newState2 (squareAt g (x,y)) (takenNeighbours (x,y) g)
    | y <- [0..width-1]
    ]
    | x <- [0..height-1]
  ]
  where height = length g
        width = if length g == 0 then 0 else length (g!!0)

part2 :: Grid -> Int
part2 = countTakenSeats . stabilize complexStep

main :: IO ()
main = getInput "input11" >>= (print . part1)

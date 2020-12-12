module Main where

data Instruction
  = Move Direction Int
  | TurnRight Int
  | Forward Int
  deriving (Show, Eq)

getInput :: FilePath -> IO [Instruction]
getInput = fmap (map parseInstruction . lines) . readFile
  where parseInstruction ('N':n) = Move North (read n)
        parseInstruction ('S':n) = Move South (read n)
        parseInstruction ('W':n) = Move West (read n)
        parseInstruction ('E':n) = Move East (read n)
        parseInstruction ('L':n) = TurnRight (360 - read n)
        parseInstruction ('R':n) = TurnRight (read n)
        parseInstruction ('F':n) = Forward (read n)

data Direction = North | South | East | West
  deriving (Show, Eq)

data Ship = Ship 
  { direction :: Direction
  , pos :: (Int, Int)
  }

turn :: Ship -> Int -> Ship
turn s 0 = s
turn (Ship d p) n = turn (Ship (singleTurn d) p) (n-1)
  where singleTurn North = East
        singleTurn East = South
        singleTurn South = West
        singleTurn West = North

move :: Instruction -> Ship -> Ship
move (TurnRight n) s = turn s (n `div` 90)
move (Forward n) s@(Ship d _) = move (Move d n) s
move (Move North n) (Ship d (x,y)) = Ship d (x, y+n)
move (Move South n) (Ship d (x,y)) = Ship d (x, y-n)
move (Move West n) (Ship d (x,y)) = Ship d (x-n, y)
move (Move East n) (Ship d (x,y)) = Ship d (x+n, y)

main :: IO ()
main = getInput "input12" >>= (print . part1)
  where part1 = distance . foldr move initialShip
        distance (Ship _ (x,y)) = abs x + abs y
        initialShip = Ship East (0, 0)

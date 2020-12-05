module Main where

import Data.List ((\\), find)

main :: IO ()
main = getReservations "input5" >>= (putStrLn . show . part2)
  where part1 = foldr max 0 . map computeId
        part2 = fmap computeId . mySeat

type Seat = (Int, Int)

type Reservations = [Seat]

getReservations :: FilePath -> IO Reservations
getReservations = fmap (map seatFromPartition . lines) . readFile

seatFromPartition :: String -> Seat
seatFromPartition s =
  case splitAt 7 s of
   (l, r) -> (rowFromPartition l, columnFromPartition r)
  where columnFromPartition = binaryToDecimal . map (== 'R')
        rowFromPartition = binaryToDecimal . map (== 'B')

mySeat :: Reservations -> Maybe Seat
mySeat r = find isInMiddle freeSeats
  where
    allPossibleSeats =
      [(row, col) | row <- [0..127], col <- [0..7]]
    freeSeats = allPossibleSeats \\ r
    takenIds = map computeId r
    isInMiddle s =
      (computeId s - 1) `elem` takenIds && 
      (computeId s + 1) `elem` takenIds

computeId :: Seat -> Int
computeId (row, column) = row * 8 + column

binaryToDecimal :: [Bool] -> Int
binaryToDecimal b = sum $ zipWith value b $ reverse indices
  where value False _ = 0
        value True i = 2^i
        indices = [0..length b - 1]

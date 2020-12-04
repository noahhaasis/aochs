module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Advent

type Passport = Map String String

requiredFields :: [String]
requiredFields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl" , "pid" ]

hasRequiredFields :: Passport -> Bool
hasRequiredFields p = all (`Map.member` p) requiredFields

isValid :: Passport -> Bool
isValid p = hasRequiredFields p && validFieldValues p
  where validFieldValues = all isValidField . Map.toList

inRange :: Int -> Int -> String -> Bool
inRange lower upper v =
  case readMaybe v of
    Just i -> lower <= i && i <= upper
    Nothing -> False

fourDigits = (== 4) . length

colors :: [String]
colors = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

isNumber :: String -> Bool
isNumber n = (/= Nothing) $ (readMaybe n :: Maybe Int)

isHex :: String -> Bool
isHex ('#' : digits) = all isHexDigit digits
  where
    isHexDigit = (`elem` hexDigits)
    hexDigits =
      [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
      , 'a', 'b', 'c', 'd', 'e', 'f'
      ]
isHex _ = False


isValidField :: (String, String) -> Bool
isValidField ("byr", v) = fourDigits v && inRange 1920 2002 v
isValidField ("iyr", v) = fourDigits v && inRange 2010 2020 v
isValidField ("eyr", v) = fourDigits v && inRange 2020 2030 v
isValidField ("hgt", v) = length v > 2 &&
  case splitAt (length v - 2) v of
    (height, "cm") -> inRange 150 193 height
    (height, "in") -> inRange 59 76 height
    _ -> False
isValidField ("hcl", v) = length v == 7 && isHex v
isValidField ("ecl", v) = v `elem` colors
isValidField ("pid", v) = length v == 9 && isNumber v
isValidField _ = True

parse :: String -> Passport
parse = Map.fromList . (map parseKeyValue) . words
  where
    parseKeyValue s =
      case split s ':' of
        [k, v] -> (k, v)
        _ -> error ("Parse error. Failed to parse '" ++ s ++ "'")


getPassports :: FilePath -> IO [Passport]
getPassports = fmap (map (parse . unlines) . flip split [] . lines) . readFile

main :: IO ()
main = getPassports "input4" >>= (putStrLn . show. part2)
  where part1 = count hasRequiredFields
        part2 = count isValid

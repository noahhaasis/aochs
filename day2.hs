module Main where


isValidPassword :: (Int, Int, Char, String) -> Bool
isValidPassword (lower, 0, c, (x:xs)) | x == c = False -- To many of 'c'
isValidPassword (lower, _, _, []) = lower <= 0 -- Check if enough characters were there
isValidPassword (lower, upper, c, (x:xs)) =
  if c == x
     then isValidPassword ((lower-1), (upper-1), c, xs)
     else isValidPassword (lower, upper, c, xs)

newPolicy :: (Int, Int, Char, String) -> Bool
newPolicy (pos1, pos2, c, s) =
  (p1 < length s && s !! p1 == c) `xor` (p2 < length s && s !! p2 == c)
    where p1 = pos1 - 1
          p2 = pos2 - 1

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

getInput :: FilePath -> IO [(Int, Int, Char, String)]
getInput = fmap (map parseLine . lines) . readFile

parseLine :: String -> (Int, Int, Char, String)
parseLine l =
  case words l of
    [first, (c:":"), third] -> (read lower, read $ drop 1 upper, c, third)
      where (lower, upper) = break (== '-') first
    _ -> error ("Parse error. Can't parse \"" ++ l ++ "\"")

main :: IO ()
main = do
  input <- getInput "input.txt"
  putStrLn (show $ length $ filter newPolicy input)

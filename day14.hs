{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as B
import Data.Word (Word64)

data Computer = Computer
  { activeMask :: String
  , memory :: Map Word64 Word64
  }
  deriving Show

setValue :: Computer -> Word64 -> Word64 -> Computer
setValue Computer {..} address value = Computer activeMask (M.insert address (maskValue activeMask value) memory)

maskValue :: String -> Word64 -> Word64
maskValue mask value = (trunkateBits 36 value .&. keepMask) .|. maskOnes
  where
    bitMask = undefined
    -- In this maske Xses are 1s
    keepMask :: Word64
    keepMask = foldr (flip B.setBit) 0 [i | (c, i) <- zip (reverse mask) [0..], c == 'X']
    maskOnes :: Word64
    maskOnes = foldr (flip B.setBit) 0 [i | (c, i) <- zip (reverse mask) [0..], c == '1']

trunkateBits :: Int -> Word64 -> Word64
trunkateBits n value = value .&. (2^n-1)

data Command
  = SetMask String
  | SetValue { address :: Word64, value :: Word64 }
  deriving Show

parseCommand :: String -> Command
parseCommand s =
  case words s of
   ["mask", _, mask] -> SetMask mask
   [left, _, value] -> SetValue (read $ drop 4 (take (length left - 1) left)) (read value)

parse :: String -> [Command]
parse = fmap parseCommand . lines

execute :: Computer -> Command -> Computer
execute Computer { .. } (SetMask m) = Computer m memory
execute c (SetValue a v) = setValue c a v

execute2 :: Computer -> Command -> Computer
execute2 Computer { .. } (SetMask m) = Computer m memory
execute2 Computer {.. } (SetValue a v) = Computer activeMask newMemory
  where newMemory = foldl (\m addr -> M.insert addr v m) memory (decodeAddress activeMask a)

decodeAddress :: String -> Word64 -> [Word64]
decodeAddress mask address = indicesToWord <$> helper maskWithBit
  where
    indicesToWord :: [Int] -> Word64
    indicesToWord = foldr (flip B.setBit) 0
    maskWithBit = [(c, B.testBit address i) | (c, i) <- zip (reverse mask) [0..]]
    -- Produce a list of lists where the inner lists
    -- contain indices of '1' and can be converted to integers
    helper :: [(Char, Bool)] -> [[Int]]
    helper [('0', b)] = if b then [[0]] else [[]]
    helper [('1', _)] = [[0]]
    helper [('X', _)] = [[0], []]
    helper (('0', True) : xs) = (0:) . fmap (+1) <$> helper xs
    helper (('0', False) : xs) = fmap (+1) <$> helper xs
    helper (('1', _) : xs) = (0:) . fmap (+1) <$> helper xs
    helper (('X', _) : xs) =
      let rest = fmap (+1) <$> helper xs
       in rest ++ fmap (0:) rest

run :: String -> Computer
run s = foldl execute2 (Computer "" M.empty) $ parse s

solution1 :: String -> Integer
solution1 = fromIntegral . sum . M.elems . memory . run

main :: IO ()
main = readFile "input" >>= (print . solution1)

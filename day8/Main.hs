module Main where

import Control.Monad.State
import Data.List (findIndices, find)
import Data.Set (Set)
import qualified Data.Set as Set

data Statement = Acc Int | Jmp Int | Nop Int
  deriving Show

type Program = [Statement]

data ExecutionContext = ExecutionContext
  { program :: Program
  , pc :: Int -- Program Counter
  , visitedIns :: Set Int
  , acc :: Int -- Accumulator
  }
  deriving Show

initialExecutionContext :: Program -> ExecutionContext
initialExecutionContext p = ExecutionContext p 0 Set.empty 0

fetch :: State ExecutionContext Statement
fetch = gets (\(ExecutionContext p pc _ _) -> p !! pc)

accIns :: Int -> State ExecutionContext ()
accIns i = modify
  (\(ExecutionContext p pc v acc) -> ExecutionContext p pc v (acc+i))

jmpIns :: Int -> State ExecutionContext ()
jmpIns n = modify
  (\(ExecutionContext p pc v acc) -> ExecutionContext p (pc+n) v acc)

incrPc :: State ExecutionContext ()
incrPc = modify
  (\(ExecutionContext p pc v acc) -> ExecutionContext p (pc+1) v acc)


executeInstruction :: State ExecutionContext ()
executeInstruction = do
  ins <- fetch
  case ins of
    Nop _ -> incrPc
    Acc i -> do accIns i; incrPc
    Jmp p -> jmpIns p

storePc :: Int -> State ExecutionContext ()
storePc n = modify
  (\(ExecutionContext p pc v acc) -> ExecutionContext p pc (Set.insert n v) acc)

terminate :: State ExecutionContext Bool
terminate = do
  s <- get
  return (pc s == length (program s))

-- Returns true if the program terminates normally and false
-- if it terminates by attempting to execute a line twice.
execute :: State ExecutionContext Bool
execute = do
  s <- get
  if pc s `Set.member` visitedIns s
    then return False
    else do
      let oldPc = pc s
      executeInstruction
      storePc oldPc
      t <- terminate
      if t then return True
           else execute

part1 :: Program -> Int
part1 p = acc $ execState execute (initialExecutionContext p)

terminates :: Program -> Bool
terminates p = evalState execute (initialExecutionContext p)

programPermutations :: Program -> [Program]
programPermutations p = map (swapAtPos p) jmpOrNopPositions
  where
    jmpOrNopPositions = findIndices jmpOrNop p
    jmpOrNop (Jmp _) = True
    jmpOrNop (Nop _) = True
    jmpOrNop _ = False
    swapAtPos p i = setAt p i newIns
      where newIns = case p !! i of
                       (Jmp n) -> Nop n
                       (Nop n) -> Jmp n

part2 :: Program -> Maybe Int
part2 p = acc . snd <$> find fst finalStates
  where
    finalStates = runState execute . initialExecutionContext
      <$> programPermutations p

setAt :: [a] -> Int -> a -> [a]
setAt (x:xs) 0 y =  y:xs
setAt (x:xs) n y = x : setAt xs (n-1) y

parseProgram :: String -> Program
parseProgram = fmap parseInstruction . lines
  where
    parseInstruction l =
      case words l of
        ["jmp", arg] -> Jmp $ parseInt arg
        ["acc", arg] -> Acc $ parseInt arg
        ["nop", arg] -> Nop $ parseInt arg
    parseInt ('+':n) = read n
    parseInt n = read n

getProgram :: FilePath -> IO Program
getProgram = fmap parseProgram . readFile

main :: IO ()
main = getProgram "input8" >>= (print . part2)


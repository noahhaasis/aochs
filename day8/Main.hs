module Main where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

data Statement = Acc Int | Jmp Int | Nop
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
    Nop -> incrPc
    Acc i -> do accIns i; incrPc
    Jmp p -> jmpIns p

storePc :: Int -> State ExecutionContext ()
storePc n = modify
  (\(ExecutionContext p pc v acc) -> ExecutionContext p pc (Set.insert n v) acc)

execute :: State ExecutionContext Int
execute = do
  s <- get
  if pc s `Set.member` visitedIns s
    then return $ acc s
    else do
      let currentPc = pc s
      executeInstruction
      storePc currentPc
      execute

part1 :: Program -> Int
part1 p = evalState execute (initialExecutionContext p)

parseProgram :: String -> Program
parseProgram = fmap parseInstruction . lines
  where
    parseInstruction l =
      case words l of
        ["jmp", arg] -> Jmp $ parseInt arg
        ["acc", arg] -> Acc $ parseInt arg
        ["nop", _] -> Nop
    parseInt ('+':n) = read n
    parseInt n = read n

getProgram :: FilePath -> IO Program
getProgram = fmap parseProgram . readFile

main :: IO ()
main = getProgram "input8" >>= (print . part1)

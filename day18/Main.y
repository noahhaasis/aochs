{
module Main where

import Data.Char
}

%name expr
%tokentype { Token }
%error { parseError }

{- Grammar for part1


Exp
  : Exp '*' Factor { $1 * $3 }
  : Exp '+' Factor { $1 + $3 }
  | Factor { $1 }

Factor
  : int { $1 }
  | '(' Exp ')' { $2 }


-}

%token
  int { TokenInt $$ }
  '+' { TokenAdd }
  '*' { TokenMul }
  '(' { TokenOP }
  ')' { TokenCP }

%%

-- Grammar for part2

Exp
  : Exp '*' Exp1 { $1 * $3 }
  | Exp1 { $1 }

Exp1
  : Exp1 '+' Factor { $1 + $3 }
  | Factor { $1 }

Factor
  : int { $1 }
  | '(' Exp ')' { $2 }

{

data Token
  = TokenInt Int
  | TokenAdd
  | TokenMul
  | TokenOP
  | TokenCP
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer ('+':xs) = TokenAdd : lexer xs
lexer ('*':xs) = TokenMul : lexer xs
lexer ('(':xs) = TokenOP : lexer xs
lexer (')':xs) = TokenCP : lexer xs
lexer ('\n':xs) = lexer xs
lexer (' ':xs) = lexer xs
lexer (n:xs) | isDigit n = lexNum (n:xs)
lexer "" = []
lexer s = error ("Failed to scan " ++ s)

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

getInput :: FilePath -> IO [Int]
getInput = fmap (map parse . lines) . readFile
  where parse = expr . lexer

main :: IO ()
main = getInput "input18" >>= (print . part1)
  where part1 = sum
}

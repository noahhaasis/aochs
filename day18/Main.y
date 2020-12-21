{
module Main where

import Data.Char
}

%name expr
%tokentype { Token }
%error { parseError }

%token
  int { TokenInt $$ }
  '+' { TokenAdd }
  '*' { TokenMul }
  '(' { TokenOP }
  ')' { TokenCP }

%%

Exp
  : Exp '*' Factor { Mul $1 $3 }
  | Exp '+' Factor { Add $1 $3 }
  | Factor { $1 }

Factor
  : int { Lit $1 }
  | '(' Exp ')' { $2 }

{

data Token
  = TokenInt Int
  | TokenAdd
  | TokenMul
  | TokenOP
  | TokenCP
  deriving (Eq, Show)

data Exp
  = Mul Exp Exp
  | Add Exp Exp
  | Lit Int
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

eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Lit n) = n


part1 = sum . map eval

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

getInput :: FilePath -> IO [Exp]
getInput = fmap (map parse . lines) . readFile
  where parse = expr . lexer

main :: IO ()
main = getInput "input18" >>= (print . part1)

}

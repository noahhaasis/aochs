{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State (State, get, modify, when, execState)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Prelude hiding (readFile)

newtype Color = Color Text
  deriving (Show, Eq, Ord)

newtype Modifier = Modifier Text
  deriving (Show, Eq, Ord)

data Bag = Bag
  { modifier :: Modifier
  , color :: Color
  } deriving (Show, Eq, Ord)

data Rule = Rule 
  { container :: Bag
  , requiredBags :: [(Int, Bag)]
  } deriving (Show, Eq)

-- Parsing

type Parser = Parsec Void Text

word :: Parser Text
word = T.pack <$> some letterChar

parseColor :: Parser Color
parseColor = Color <$> word

parseModifier :: Parser Modifier
parseModifier = Modifier <$> word

parseBag :: Parser Bag
parseBag = do
  m <- parseModifier
  _ <- space
  c <- parseColor
  _ <- space
  _ <- string "bags" <|> string "bag"
  return $ Bag m c

parseBagContent :: Parser [(Int, Bag)]
parseBagContent = nothing <|> bags
  where
    nothing = [] <$ "no other bags."
    bags = do
      bs <- singleBag `sepBy` comma
      _ <- char '.'
      return bs
    comma = string ", "
    singleBag = do
      n <- number
      _ <- space
      b <- parseBag
      return (n, b)
    number = (read <$> some digitChar) :: Parser Int

parseRule :: Parser Rule
parseRule = do
  outer <- parseBag
  _ <- " contain "
  content <- parseBagContent
  return $ Rule outer content

parseInput :: FilePath -> IO (Maybe [Rule])
parseInput = fmap (mapM (parseMaybe parseRule) . T.lines) . readFile

bagsPossibleContaining :: Bag -> [Rule] -> Set Bag
bagsPossibleContaining b r = execState (traverseRules False b r) Set.empty

traverseRules :: Bool -> Bag -> [Rule] -> State (Set Bag) ()
traverseRules addSelf b rules = do
  s <- get
  if b `Set.member` s
    then return ()
    else do
      let nextNodes = depthOneBagsContaining b rules
      when addSelf (modify (Set.insert b))
      mapM_ (\n -> traverseRules True n rules) nextNodes

depthOneBagsContaining :: Bag -> [Rule] -> Set Bag
depthOneBagsContaining b rules = Set.fromList
  [outer | Rule outer inner <- rules, any ((==) b . snd) inner]

part1 :: [Rule] -> Int
part1 rules = Set.size $ bagsPossibleContaining myBag rules
  where myBag = Bag (Modifier "shiny") (Color "gold")

main :: IO ()
main = parseInput "input7" >>= (print . fmap part1)

module Parser.Tree (Tree(..)) where

data Tree =
    Int Integer
  | Real Double
  deriving (Show, Eq)

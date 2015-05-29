module Parser.Tree (Tree(..)) where

data Tree =
    Int Integer
  | Real Double
  | Str String
  deriving (Show, Eq)

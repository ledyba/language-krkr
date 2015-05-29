module Parser.Tree (Tree(..)) where

data Tree =
    Int Integer
  | Real Double
  | Str String
  | Ident String
  deriving (Show, Eq)

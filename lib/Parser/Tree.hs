module Parser.Tree (Tree(..),Expr(..),Identifer(..)) where

newtype Identifer = Identifer String deriving (Show, Eq)

data Expr =
      Bin String Expr Expr
    | PreUni String Expr
    | PostUni String Expr
    | Tri Expr Expr Expr
    | Cast Identifer Expr
    | Int Integer
    | Real Double
    | Str String
    | Ident Identifer
    | Array [Expr]
    | Dict [(String, Expr)]
    | Null
    | WithThis
  deriving (Show, Eq)

data Tree =
    Cont Tree Tree
  | Expr Expr
  deriving (Show, Eq)

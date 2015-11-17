module Parser.Tree (Tree(..),Expr(..),Identifer(..),ApplyArg(..)) where

newtype Identifer = Identifer String deriving (Show, Eq)

data Expr =
      Bin String Expr Expr
    | PreUni String Expr
    | PostUni Expr String
    | Tri Expr Expr Expr
    | Cast Identifer Expr
    | Int Integer
    | Real Double
    | Str String
    | Ident Identifer
    | Array [Expr]
    | Dict [(String, Expr)]
    | Index Expr Expr
    | Call Expr [ApplyArg]
    | Dot Expr Identifer
    | Null
    | WithThis
  deriving (Show, Eq)

data ApplyArg =
    ApplyLeft
  | ApplyArray Expr
  | ApplyExpr Expr
  deriving (Show, Eq)

data Tree =
    Cont Tree Tree
  | Expr Expr
  deriving (Show, Eq)

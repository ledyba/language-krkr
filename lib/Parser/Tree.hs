module Parser.Tree (Stmt(..),Expr(..),Identifer(..),ApplyArg(..)) where

newtype Identifer = Identifer String deriving (Show, Eq)

data Stmt =
    If Expr Stmt (Maybe Stmt)
  | Switch Expr [(Expr, [Stmt])] (Maybe [Stmt])
  | While Expr Stmt
  | Break
  | With
  | Try Stmt Identifer Stmt
  | For
  | Throw
  | Return
  | Prop
  | Class
  | Func
  | Block
  | Var
  | Exec Expr
  deriving (Show, Eq)

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

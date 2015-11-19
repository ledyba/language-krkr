module Parser.Tree (Stmt(..),Expr(..),Identifer(..),ApplyArg(..)) where

newtype Identifer = Identifer String deriving (Show, Eq)

data Stmt =
    If Expr Stmt (Maybe Stmt)
  | Switch Expr [(Expr, [Stmt])] (Maybe [Stmt])
  | While Expr Stmt
  | Break
  | With
  | Try Stmt Identifer Stmt
  | Throw Expr
  | For Expr Expr Expr Stmt
  | Continue
  | Return Expr
  | Prop
  | Class
  | Func
  | Block [Stmt]
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

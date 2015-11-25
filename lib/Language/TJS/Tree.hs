module Language.TJS.Tree (
   Stmt(..)
  ,Expr(..)
  ,Identifer(..)
  ,ApplyArg(..)
  ,FuncArg(..)
  ,SrcSpan(..)
  -- module Text.ParserCombinators.Parsec.Pos
  , SourcePos
  , SourceName, Line, Column
  , sourceName, sourceLine, sourceColumn
) where

import Text.ParserCombinators.Parsec (
   SourcePos
  ,SourceName, Line, Column
  ,sourceName, sourceLine, sourceColumn)

data SrcSpan = SrcSpan SourcePos SourcePos | NoSrcSpan deriving (Show)
instance Eq SrcSpan where
  NoSrcSpan     == _             = True
  _             == NoSrcSpan     = True
  (SrcSpan a b) == (SrcSpan c d) = (==) a c && (==) b d

newtype Identifer = Identifer String deriving (Show, Eq)

data Stmt =
    If       Expr Stmt (Maybe Stmt) SrcSpan
  | Switch   Expr [(Expr, [Stmt])] (Maybe [Stmt]) SrcSpan
  | While    Expr Stmt SrcSpan
  | Break    SrcSpan
  | With     Expr Stmt SrcSpan
  | Try      Stmt Identifer Stmt SrcSpan
  | Throw    Expr SrcSpan
  | For      Expr Expr Expr Stmt SrcSpan
  | Continue SrcSpan
  | Return   Expr SrcSpan
  | Prop     Identifer (Maybe Stmt) (Maybe (Identifer, Stmt)) SrcSpan
  | Class    Identifer [Stmt] SrcSpan
  | Func     (Maybe Identifer) [FuncArg] Stmt SrcSpan
  | Block    [Stmt] SrcSpan
  | Var      [(Identifer,Maybe Expr)] SrcSpan
  | Exec     Expr SrcSpan
  deriving (Show, Eq)

data Expr =
      Bin      String Expr Expr SrcSpan
    | PreUni   String Expr SrcSpan
    | PostUni  Expr String SrcSpan
    | Tri      Expr Expr Expr SrcSpan
    | Cast     Identifer Expr SrcSpan
    | Int      Integer SrcSpan
    | Real     Double SrcSpan
    | Str      String SrcSpan
    | Ident    Identifer SrcSpan
    | Array    [Expr] SrcSpan
    | Dict     [(String, Expr)] SrcSpan
    | Index    Expr Expr SrcSpan
    | Call     Expr [ApplyArg] SrcSpan
    | Dot      Expr Identifer SrcSpan
    | Null     SrcSpan
    | WithThis SrcSpan
  deriving (Show, Eq)

data ApplyArg =
    ApplyLeft
  | ApplyArray Expr
  | ApplyExpr Expr
  deriving (Show, Eq)

data FuncArg =
    FuncLeft
  | FuncArray Identifer
  | FuncArg Identifer (Maybe Expr)
  deriving (Show, Eq)

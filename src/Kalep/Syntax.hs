module Kalep.Syntax
  ( Name
  , Literal(..)
  , Expr(..)
  , Stmt(..)
  , Op(..)
  )
where

type Name = String

data Literal
  = IntL Integer
  | DoubleL Double
  | StringL String
  deriving (Eq, Ord, Show)

data Expr
  = LitExpr Literal
  | Var Name
  | BinExpr Op Expr Expr
  | Call Name [Expr]
  | Statement Stmt
  deriving (Eq, Ord, Show)

data Stmt
  = Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Mult
  | Divide
  deriving (Eq, Ord, Show)

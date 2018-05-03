module Syntax where

data Expr
  = Int Integer
  | Add Expr Expr
  | IntEq Expr Expr
  | Unit
  | Bool Bool
  | Not Expr
  | Ann Expr Type
  | Function Statement
  | Var String
  deriving (Eq, Show)

data Statement
  = If Statement Statement Statement
  | While Statement Statement
  | Seq Statement Statement
  | NewRef Expr
  | Read Expr
  | Write Expr Expr
  | Assign String (Maybe Type) Statement
  | Expr Expr
  | Pass
  deriving (Eq, Show)

data Type
  = TyBool
  | TyInt
  | TyArr Type Type
  | TyRef Type
  | TyUnit
  | TyUnknown
  deriving (Eq, Show)

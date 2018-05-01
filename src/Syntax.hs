module Syntax where

data Expr
  = Int Integer
  | Unit
  | Bool Bool
  | Ann Expr Type
  | Function Statement
  deriving (Eq, Show)

data Statement
  = If Expr Statement Statement
  | While Expr Statement
  | Seq Statement Statement
  | Ref Expr
  | Read Expr
  | Assign String Statement
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

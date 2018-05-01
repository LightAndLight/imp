module Syntax where

data Expr
  = Int Int
  | Bool Bool
  | Ann Expr Type
  deriving (Eq, Show)

data Statement
  = If Expr Statement Statement
  | While Expr Statement
  | Seq Statement Statement
  | Pass
  deriving (Eq, Show)

data Type
  = TyBool
  | TyInt
  deriving (Eq, Show)

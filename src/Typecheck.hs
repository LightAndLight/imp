module Typecheck where

import Control.Monad (unless)

import Syntax (Expr(..), Statement(..), Type(..))

data TypeError
  = TypeMismatch
      Type {- expected -}
      Type {- actual   -}
  deriving (Eq, Show)

infer :: Expr -> Either TypeError Type
infer Int{} = pure TyInt
infer Bool{} = pure TyBool
infer (Ann expr ty) = do
  ty' <- infer expr
  unless (ty == ty') . Left $ TypeMismatch ty ty'
  pure ty

tcStatement :: Statement -> Either TypeError ()
tcStatement (If cond st_if st_else) = do
  cond_ty <- infer cond
  unless (cond_ty == TyBool) . Left $ TypeMismatch TyBool cond_ty
  tcStatement st_if
  tcStatement st_else
tcStatement (While cond body) = do
  cond_ty <- infer cond
  unless (cond_ty == TyBool) . Left $ TypeMismatch TyBool cond_ty
  tcStatement body
tcStatement (Seq st1 st2) = tcStatement st1 *> tcStatement st2
tcStatement Pass = pure ()

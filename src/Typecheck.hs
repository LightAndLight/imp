{-# language MultiParamTypeClasses, FlexibleContexts #-}
module Typecheck where

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))

import Syntax (Expr(..), Statement(..), Type(..))

data TypeError
  = TypeMismatch
      Type {- expected -}
      Type {- actual   -}
  deriving (Eq, Show)

infer :: MonadError TypeError m => Expr -> m Type
infer Unit = pure TyUnit
infer Int{} = pure TyInt
infer Bool{} = pure TyBool
infer (Ann expr ty) = do
  ty' <- infer expr
  unless (ty == ty') . throwError $ TypeMismatch ty ty'
  pure ty
infer (Function body) = do
  retTy <- tcStatement body
  pure $ TyArr TyUnit retTy

tcStatement
  :: MonadError TypeError m
  => Statement
  -> m Type
tcStatement (If cond st_if st_else) = do
  cond_ty <- infer cond
  unless (cond_ty == TyBool) . throwError $ TypeMismatch TyBool cond_ty

  if_ty <- tcStatement st_if
  else_ty <- tcStatement st_else
  unless (if_ty == else_ty) . throwError $ TypeMismatch if_ty else_ty

  pure if_ty
tcStatement (While cond body) = do
  cond_ty <- infer cond
  unless (cond_ty == TyBool) . throwError $ TypeMismatch TyBool cond_ty
  tcStatement body
tcStatement (Seq st1 st2) = tcStatement st1 *> tcStatement st2
tcStatement Pass = pure TyUnit
tcStatement (Expr expr) = infer expr
tcStatement (Ref expr) = TyRef <$> infer expr
tcStatement (Read expr) = do
  ty <- infer expr
  case ty of
    TyRef ty' -> pure ty'
    _ -> throwError $ TypeMismatch (TyRef TyUnknown) ty

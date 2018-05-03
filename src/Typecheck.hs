{-# language MultiParamTypeClasses, FlexibleContexts #-}
module Typecheck where

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..), modify)
import Data.Map (Map)

import qualified Data.Map as Map

import Syntax (Expr(..), Statement(..), Type(..))

data TypeError
  = TypeMismatch
      Type {- expected -}
      Type {- actual   -}
  | NotFound String
  deriving (Eq, Show)

infer
  :: ( MonadState (Map String Type) m
     , MonadError TypeError m
     )
  => Expr -> m (Expr, Type)
infer v@(Var name) = do
  ctxt <- get
  maybe
    (throwError $ NotFound name)
    (\ty -> pure (Ann v ty, ty))
    (Map.lookup name ctxt)
infer Unit = let ty = TyUnit in pure (Ann Unit ty, ty)
infer i@Int{} = let ty = TyInt in pure (Ann i ty, ty)
infer b@Bool{} = let ty = TyBool in pure (Ann b ty, ty)
infer e@(Ann expr ty) = do
  (_, ty') <- infer expr
  unless (ty == ty') . throwError $ TypeMismatch ty ty'
  pure (e, ty)
infer (Function body) = do
  (body', body_ty) <- tcStatement body
  let ty = TyArr TyUnit body_ty
  pure (Ann (Function body') ty, ty)

tcStatement
  :: ( MonadError TypeError m
     , MonadState (Map String Type) m
     )
  => Statement
  -> m (Statement, Type)
tcStatement (Assign name st) = do
  (st', st_ty) <- tcStatement st
  modify $ Map.insert name st_ty
  pure (Assign name st', TyUnit)
tcStatement (If cond st_if st_else) = do
  (cond', cond_ty) <- infer cond
  unless (cond_ty == TyBool) . throwError $ TypeMismatch TyBool cond_ty

  (st_if', if_ty) <- tcStatement st_if
  (st_else', else_ty) <- tcStatement st_else
  unless (if_ty == else_ty) . throwError $ TypeMismatch if_ty else_ty

  pure (If cond' st_if' st_else', if_ty)
tcStatement (While cond body) = do
  (cond', cond_ty) <- infer cond
  unless (cond_ty == TyBool) . throwError $ TypeMismatch TyBool cond_ty
  (body', body_ty) <- tcStatement body
  pure (While cond' body', body_ty)
tcStatement (Seq st1 st2) = do
  (st1', _) <- tcStatement st1
  (st2', st2_ty) <- tcStatement st2
  pure (Seq st1' st2', st2_ty)
tcStatement Pass = pure (Pass, TyUnit)
tcStatement (Expr expr) = do
  (expr', expr_ty) <- infer expr
  pure (Expr expr', expr_ty)
tcStatement (NewRef expr) = do
  (expr', expr_ty) <- infer expr
  pure (NewRef expr', TyRef expr_ty)
tcStatement (Read expr) = do
  (expr', expr_ty) <- infer expr
  case expr_ty of
    TyRef ty -> pure (Read expr', ty)
    _ -> throwError $ TypeMismatch (TyRef TyUnknown) expr_ty

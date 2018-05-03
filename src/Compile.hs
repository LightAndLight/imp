{-# language MultiParamTypeClasses, FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
module Compile where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState(..), modify, evalState)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import LLVM.AST (Operand)
import LLVM.AST.Type (i1, i32, ptr)
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Target (withHostTargetMachine)
import LLVM.IRBuilder.Constant (int32, bit)
import LLVM.IRBuilder.Instruction (condBr, br, alloca, load, ret, store, add, icmp)
import LLVM.IRBuilder.Module (ModuleBuilderT, buildModuleT, function)
import LLVM.IRBuilder.Monad (IRBuilderT, block)
import LLVM.Module (File, withModuleFromAST, writeBitcodeToFile, writeTargetAssemblyToFile)
import LLVM.Pretty (ppllvm)

import qualified Data.Map as Map
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM

import Syntax (Expr(..), Statement(..), Type(..))

moduleName :: IsString s => s
moduleName = "module"

compileBitcode :: Statement -> File -> IO ()
compileBitcode st fp =
  withContext $ \ctxt ->
  withModuleFromAST
    ctxt
    (flip evalState Map.empty . buildModuleT moduleName $ cgModule st)
    (writeBitcodeToFile fp)

compileAsm :: Statement -> File -> IO ()
compileAsm st fp =
  withHostTargetMachine $ \targetMachine ->
  withContext $ \ctxt ->
  withModuleFromAST
    ctxt
    (flip evalState Map.empty . buildModuleT moduleName $ cgModule st)
    (writeTargetAssemblyToFile targetMachine fp)

renderCode :: Statement -> Text
renderCode =
  toStrict . ppllvm . flip evalState Map.empty . buildModuleT moduleName . cgModule

cgExpr :: (MonadFix m, MonadState (Map String Operand) m) => Expr -> IRBuilderT m Operand
cgExpr (Not expr) = do
  expr_res <- cgExpr expr
  c <- bit 0
  icmp LLVM.EQ expr_res c
cgExpr (IntEq expr1 expr2) = do
  expr1_res <- cgExpr expr1
  expr2_res <- cgExpr expr2
  icmp LLVM.EQ expr1_res expr2_res
cgExpr (Add expr1 expr2) = do
  expr1_res <- cgExpr expr1
  expr2_res <- cgExpr expr2
  add expr1_res expr2_res
cgExpr (Var name) = do
  env <- lift get
  let
    reference =
      fromMaybe
      (error $ name <> " not found in environment")
      (Map.lookup name env)
  load reference 0
cgExpr Unit = bit 0
cgExpr (Int i) = int32 i
cgExpr (Bool b) = if b then bit 1 else bit 0
cgExpr (Ann e _) = cgExpr e
cgExpr (Function body) = cgStatement body

llvmTypeExpr :: Expr -> LLVM.Type
llvmTypeExpr (Ann _ ty) = llvmTypeType ty
llvmTypeExpr _ = error "expression is missing a type annotation"

llvmTypeType :: Type -> LLVM.Type
llvmTypeType TyInt = i32
llvmTypeType TyBool = i1
llvmTypeType TyUnit = i1
llvmTypeType (TyArr _ _) = undefined
llvmTypeType (TyRef ty) = ptr (llvmTypeType ty)
llvmTypeType TyUnknown = undefined

llvmTypeStmt :: Statement -> LLVM.Type
llvmTypeStmt (If _ st _) = llvmTypeStmt st
llvmTypeStmt (While _ st) = llvmTypeStmt st
llvmTypeStmt (Seq _ st) = llvmTypeStmt st

llvmTypeStmt (NewRef (Ann _ ty)) = ptr (llvmTypeType ty)
llvmTypeStmt (NewRef _) = error "NewRef's target is missing a type annotation"

llvmTypeStmt (Read (Ann _ (TyRef ty))) = llvmTypeType ty
llvmTypeStmt (Read (Ann _ _)) = error "Read's target is not a reference"
llvmTypeStmt (Read _) = error "Read's target is missing a type annotation"

llvmTypeStmt Write{} = llvmTypeExpr Unit

llvmTypeStmt Assign{} = llvmTypeExpr Unit

llvmTypeStmt (Expr (Ann _ ty)) = llvmTypeType ty
llvmTypeStmt (Expr _) = error "Expr's target is missing a type annotation"

llvmTypeStmt Pass = llvmTypeExpr Unit

stmtType :: Statement -> Type
stmtType (If _ st _) = stmtType st
stmtType (While _ st) = stmtType st
stmtType (Seq _ st) = stmtType st

stmtType (NewRef (Ann _ ty)) = TyRef ty
stmtType (NewRef _) = error "NewRef's target is missing a type annotation"

stmtType (Read (Ann _ (TyRef ty))) = ty
stmtType (Read (Ann _ _)) = error "Read's target is not a reference"
stmtType (Read _) = error "Read's target is missing a type annotation"

stmtType Write{} = TyUnit

stmtType Assign{} = TyUnit

stmtType (Expr (Ann _ ty)) = ty
stmtType (Expr _) = error "Expr's target is missing a type annotation"

stmtType Pass = TyUnit

cgStatement
  :: (MonadFix m, MonadState (Map String Operand) m)
  => Statement
  -> IRBuilderT m Operand
cgStatement (Assign name st) = do
  st_res <- cgStatement st
  reference <- alloca (llvmTypeStmt st) Nothing 0
  store reference 0 st_res
  lift $ modify (Map.insert name reference)
  cgExpr Unit
cgStatement (Write reference value) = do
  ref_res <- cgExpr reference
  val_res <- cgExpr value
  store ref_res 0 val_res
  cgExpr Unit
cgStatement (Read expr) = do
  expr_res <- cgExpr expr
  load expr_res 0
cgStatement (NewRef expr) = do
  expr_res <- cgExpr expr
  reference <- alloca (llvmTypeExpr expr) Nothing 0
  store reference 0 expr_res
  pure reference
cgStatement (If cond st_if st_else) = mdo
  res <- alloca (llvmTypeStmt st_if) Nothing 0
  cond_res <- cgStatement cond
  condBr cond_res when_true when_false

  when_true <- block
  st_if_res <- cgStatement st_if
  store res 0 st_if_res
  br after

  when_false <- block
  st_else_res <- cgStatement st_else
  store res 0 st_else_res
  br after

  after <- block
  load res 0
cgStatement (While cond body) = mdo
  br start
  start <- block
  cond_res <- cgStatement cond
  condBr cond_res continue end

  continue <- block
  body_value <- cgStatement body
  br start

  end <- block
  pure body_value
cgStatement (Seq st1 st2) =
  cgStatement st1 *>
  cgStatement st2
cgStatement Pass = cgExpr Unit
cgStatement (Expr expr) = cgExpr expr

cgModule
  :: (MonadFix m, MonadState (Map String Operand) m)
  => Statement
  -> ModuleBuilderT m Operand
cgModule st =
  function "main" [] i32 $
  \_ -> do
    _ <- block
    cgStatement st >>= ret

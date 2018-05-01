{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
module Compile where

import Control.Monad.Fix (MonadFix)
import Data.Functor (($>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import LLVM.AST (Operand)
import LLVM.AST.Type (i1, i32)
import LLVM.Internal.Context (withContext)
import LLVM.Internal.Target (withHostTargetMachine)
import LLVM.IRBuilder.Constant (int32, bit)
import LLVM.IRBuilder.Instruction (condBr, br, alloca, load)
import LLVM.IRBuilder.Module (ModuleBuilder, buildModule, function)
import LLVM.IRBuilder.Monad (IRBuilderT, block)
import LLVM.Module (File, withModuleFromAST, writeBitcodeToFile, writeTargetAssemblyToFile)
import LLVM.Pretty (ppllvm)

import qualified LLVM.AST.Type as LLVM

import Syntax (Expr(..), Statement(..))

moduleName :: IsString s => s
moduleName = "module"

compileBitcode :: Statement -> File -> IO ()
compileBitcode st fp =
  withContext $ \ctxt ->
  withModuleFromAST
    ctxt
    (buildModule moduleName $ cgModule st)
    (writeBitcodeToFile fp)

compileAsm :: Statement -> File -> IO ()
compileAsm st fp =
  withHostTargetMachine $ \targetMachine ->
  withContext $ \ctxt ->
  withModuleFromAST
    ctxt
    (buildModule moduleName $ cgModule st)
    (writeTargetAssemblyToFile targetMachine fp)

renderCode :: Statement -> Text
renderCode = toStrict . ppllvm . buildModule moduleName . cgModule

cgExpr :: MonadFix m => Expr -> IRBuilderT m Operand
cgExpr Unit = bit 0
cgExpr (Int i) = int32 i
cgExpr (Bool b) = if b then bit 1 else bit 0
cgExpr (Ann e _) = cgExpr e
cgExpr (Function body) = cgStatement body

llvmTypeExpr :: Expr -> LLVM.Type
llvmTypeExpr Int{} = i32
llvmTypeExpr Unit = i1
llvmTypeExpr Bool{} = i1
llvmTypeExpr (Ann expr _) = llvmTypeExpr expr
llvmTypeExpr Function{} = undefined

llvmTypeStmt :: Statement -> LLVM.Type
llvmTypeStmt (If _ st _) = llvmTypeStmt st
llvmTypeStmt (While _ st) = llvmTypeStmt st
llvmTypeStmt (Seq _ st) = llvmTypeStmt st
llvmTypeStmt (Ref expr) = _ -- pointer thingy
llvmTypeStmt (Read expr) = _
llvmTypeStmt Assign{} = llvmTypeExpr Unit
llvmTypeStmt (Expr expr) = llvmTypeExpr expr
llvmTypeStmt Pass = llvmTypeExpr Unit

cgStatement :: MonadFix m => Statement -> IRBuilderT m Operand
cgStatement (Assign name value) = _
cgStatement (Read expr) = do
  expr_res <- cgExpr expr
  load expr_res 0
cgStatement (Ref expr) = do
  expr_res <- cgExpr expr
  alloca (llvmTypeExpr expr) (Just expr_res) 1
cgStatement (If cond st_if st_else) = mdo
  cond_res <- cgExpr cond
  condBr cond_res when_true when_false

  when_true <- block
  _ <- cgStatement st_if
  br after

  when_false <- block
  _ <- cgStatement st_else
  br after

  after <- block
  cgExpr Unit
cgStatement (While cond body) = mdo
  start <- block
  cond_res <- cgExpr cond
  condBr cond_res continue end

  continue <- block
  _ <- cgStatement body
  br start

  end <- block
  cgExpr Unit
cgStatement (Seq st1 st2) =
  cgStatement st1 *>
  cgStatement st2 *>
  cgExpr Unit
cgStatement Pass = cgExpr Unit
cgStatement (Expr expr) = cgExpr expr *> cgExpr Unit

cgModule :: Statement -> ModuleBuilder Operand
cgModule st = function "main" [] i32 (\_ -> cgStatement st $> ())

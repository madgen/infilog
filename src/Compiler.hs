module Compiler (compile) where

import qualified AST as A
import qualified IntermediateRepresentation as IR
import Data.Maybe (catMaybes)
import Prelude hiding (id, head, pred)
import qualified Control.Monad.Trans.State.Strict as S

compile :: A.Program -> IR.Program
compile pr =
  catMaybes $ S.evalState (traverse compileEntity pr) 0

compileEntity :: A.Entity -> FreshIDM (Maybe IR.Clause)
compileEntity (A.EClause clause) = Just <$> compileClause clause
compileEntity (A.EDeclaration _) = pure Nothing

compileClause :: A.Clause -> FreshIDM IR.Clause
compileClause (A.Clause head body) =
  IR.Clause <$> compileAtom head <*> traverse compileAtom body

compileAtom :: A.Atom -> FreshIDM IR.Atom
compileAtom (A.Atom pred terms) = do
  IR.Atom pred <$> traverse compileTerm terms

compileTerm :: A.Term -> FreshIDM IR.Term
compileTerm (A.Sym sym) = pure $ IR.Sym sym
compileTerm (A.Var var) = pure $ IR.Var $ compileVariable var
compileTerm (A.Composite _ty _cstr terms) = do
  id <- freshID
  _terms' <- traverse compileTerm terms
  pure $ IR.Id id

compileVariable :: A.Variable -> IR.Variable
compileVariable (A.Variable var) = IR.Variable var 0

type FreshIDM a = S.State Int a

freshID :: S.State Int IR.Identifier
freshID = do
  i <- S.get
  S.put $ i + 1
  pure (IR.Identifier i)
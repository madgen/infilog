module Compiler (compile) where

import qualified AST as A
import qualified IntermediateRepresentation as IR
import Data.Maybe (mapMaybe)
import Prelude hiding (head, pred)

compile :: A.Program -> IR.Program
compile = mapMaybe compileEntity

compileEntity :: A.Entity -> Maybe IR.Clause
compileEntity (A.EClause clause) = Just $ compileClause clause
compileEntity (A.EDeclaration _) = Nothing

compileClause :: A.Clause -> IR.Clause
compileClause (A.Clause head body) = IR.Clause (compileAtom head) (compileAtom <$> body)

compileAtom :: A.Atom -> IR.Atom
compileAtom (A.Atom pred terms) = IR.Atom pred (compileTerm <$> terms)

compileTerm :: A.Term -> IR.Term
compileTerm (A.Sym sym) = IR.Sym sym
compileTerm (A.Var var) = IR.Var $ compileVariable var
compileTerm (A.Composite _ _) = error "not yet handled"

compileVariable :: A.Variable -> IR.Variable
compileVariable (A.Variable var) = IR.Variable var 0

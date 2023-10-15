{-# LANGUAGE OverloadedStrings #-}
module Compiler (compile) where

import qualified AST as A
import qualified IntermediateRepresentation as IR
import Data.Maybe (catMaybes)
import Prelude hiding (id, head, pred)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Text as T
import Decl (DeclStore, TypeInfo (_numberOfParameters))
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as M
import Control.Monad (replicateM)
import Data.Coerce (coerce)

type CompileM a = FreshMT (R.Reader DeclStore) a

compile :: DeclStore -> A.Program -> IR.Program
compile declStore pr = synthClauses <> clauses
  where
    synthClauses = (`IR.Clause` []) <$> concat synthAtomss
    (clauses, synthAtomss) =
      unzip $ catMaybes $ (`R.runReader` declStore) $ S.evalStateT (traverse compileEntity pr) 0

compileEntity :: A.Entity -> CompileM (Maybe (IR.Clause, [IR.Atom]))
compileEntity (A.EClause clause) = Just <$> compileClause clause
compileEntity (A.EDeclaration _) = pure Nothing

compileClause :: A.Clause -> CompileM (IR.Clause, [IR.Atom])
compileClause (A.Clause head body) = do
  (head', synthAtoms) <- compileAtom head
  (body', synthAtomss) <- unzip <$> traverse compileAtom body
  let synthAtoms' = concat $ synthAtoms : synthAtomss
  let body'' = synthAtoms' <> body'
  pure (IR.Clause head' body'', synthAtoms')

compileAtom :: A.Atom -> CompileM (IR.Atom, [IR.Atom])
compileAtom (A.Atom pred terms) = do
  (terms', atomss) <- unzip <$> traverse compileTerm terms
  pure (IR.Atom pred terms', concat atomss)

compileTerm :: A.Term -> CompileM (IR.Term, [IR.Atom])
compileTerm (A.Sym sym) = pure (IR.Sym sym, [])
compileTerm (A.Var var) = pure (IR.Var $ compileVariable var, [])
compileTerm (A.Composite ty cstr terms) = do
  id <- IR.Id <$> freshID
  (terms', atomss) <- unzip <$> traverse compileTerm terms
  atom <- mkAtom ty id cstr terms'
  pure (id, atom : concat atomss)

mkAtom :: A.TyName -> IR.Term -> A.Constructor -> [IR.Term] -> CompileM IR.Atom
mkAtom tyName id cstr terms = do
  declStore <- lift R.ask
  fillerTerms <- do
    case tyName `M.lookup` declStore of
      Just typeInfo ->
        fmap IR.Var <$> replicateM (_numberOfParameters typeInfo - length terms) freshVar
      Nothing -> error $ "ICE: undeclared type `" <> show tyName <> "` is used in a constructor"
  let pred = coerce tyName
  let cstrSym = IR.Sym (coerce cstr)
  pure $ IR.Atom pred $ id : cstrSym : terms <> fillerTerms

compileVariable :: A.Variable -> IR.Variable
compileVariable (A.Variable var) = IR.Variable var 0

type FreshMT m a = S.StateT Int m a

freshID :: CompileM IR.Identifier
freshID = do
  i <- S.get
  S.put $ i + 1
  pure (IR.Identifier i)

freshVar :: CompileM IR.Variable
freshVar = do
  i <- S.get
  S.put $ i + 1
  pure (IR.Variable ("iv" <> T.pack (show i)) 0)
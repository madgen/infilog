{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Compiler (compile, ADTMap) where

import qualified AST as A
import qualified IntermediateRepresentation as IR
import Prelude hiding (id, head, pred)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Text as T
import Decl (DeclStore, TypeInfo (_numberOfParameters))
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as M
import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Control.Arrow (Arrow(first))
import qualified Control.Monad.Trans.Writer.Strict as W

type CompileMT = ADTMT (FreshMT (R.Reader DeclStore))
type CompileM a = CompileMT a

compile :: DeclStore -> A.Program -> (IR.Program, ADTMap)
compile declStore pr = first concat
                     $ (`R.runReader` declStore)
                     $ (`S.evalStateT` 0)
                     $ (`S.runStateT` M.empty)
                     $ traverse compileEntity pr

compileEntity :: A.Entity -> CompileM [IR.Clause]
compileEntity (A.EClause clause) = compileClause clause
compileEntity (A.EDeclaration _) = pure []

compileClause :: A.Clause -> CompileM [IR.Clause]
compileClause (A.Clause head body)
  | A.Atom pred [term] <- head, isTabulate pred = do
    fmap (`IR.Clause` []) <$> tabulate term
  | otherwise = do
  (head', synthAtoms) <- compileAtom head
  (body', synthAtomss) <- unzip <$> traverse compileAtom body
  let synthAtoms' = concat $ synthAtoms : synthAtomss
  let body'' = synthAtoms' <> body'
  pure [IR.Clause head' body'']

tabulate :: A.Term -> CompileM [IR.Atom]
tabulate term = snd <$> W.runWriterT (go term)
  where
  go :: A.Term -> W.WriterT [IR.Atom] CompileMT IR.Term
  go A.Var{} = error "ICE: there shouldn't be any variables in a tabulated term."
  go (A.Sym sym) = pure $ IR.Sym (coerce sym)
  go t@(A.Composite ty cstr terms) = do
    id <- lift . lift $ freshID
    lift $ remember id t
    terms' <- traverse go terms
    atom <- lift $ mkAtom ty (IR.Id id) cstr terms'
    W.tell [atom]
    pure $ IR.Id id

isTabulate :: A.Predicate -> Bool
isTabulate = (== A.Predicate "tabulate")

compileAtom :: A.Atom -> CompileM (IR.Atom, [IR.Atom])
compileAtom (A.Atom pred terms) = do
  (terms', atomss) <- unzip <$> traverse compileTerm terms
  pure (IR.Atom pred terms', concat atomss)

compileTerm :: A.Term -> CompileM (IR.Term, [IR.Atom])
compileTerm (A.Sym sym) = pure (IR.Sym sym, [])
compileTerm (A.Var var) = pure (IR.Var $ compileVariable var, [])
compileTerm (A.Composite ty cstr terms) = do
  var <- lift freshVar
  (terms', atomss) <- unzip <$> traverse compileTerm terms
  atom <- mkAtom ty (IR.Var var) cstr terms'
  pure (IR.Var var, atom : concat atomss)

mkAtom :: A.TyName -> IR.Term -> A.Constructor -> [IR.Term] -> CompileM IR.Atom
mkAtom tyName id cstr terms = do
  declStore <- lift . lift $ R.ask
  fillerTerms <- do
    case tyName `M.lookup` declStore of
      Just typeInfo ->
            fmap IR.Var
        <$> replicateM (_numberOfParameters typeInfo - length terms) (lift freshVar)
      Nothing -> error $ "ICE: undeclared type `" <> show tyName <> "` is used in a constructor"
  let pred = coerce tyName
  let cstrSym = IR.Sym (coerce cstr)
  pure $ IR.Atom pred $ id : cstrSym : terms <> fillerTerms

compileVariable :: A.Variable -> IR.Variable
compileVariable (A.Variable var) = IR.Variable var 0

type FreshMT m = S.StateT Int m

freshID :: Monad m => FreshMT m IR.Identifier
freshID = do
  i <- S.get
  S.put $ i + 1
  pure (IR.Identifier i)

freshVar :: Monad m => FreshMT m IR.Variable
freshVar = do
  i <- S.get
  S.put $ i + 1
  pure (IR.Variable ("iv" <> T.pack (show i)) 0)

type ADTMT m = S.StateT ADTMap m
type ADTMap = M.Map IR.Identifier A.Term

remember :: Monad m => IR.Identifier -> A.Term -> ADTMT m ()
remember id = S.modify . M.insert id
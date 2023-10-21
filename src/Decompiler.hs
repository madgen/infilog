module Decompiler (decompileAtom) where

import Prelude hiding (id, pred)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import qualified IntermediateRepresentation as IR
import qualified AST as A
import qualified Data.Text as T
import qualified Decl as D

type ADTMap = M.Map IR.Identifier A.Term

decompileAtom :: D.DeclStore -> ADTMap -> IR.Atom -> A.Atom
decompileAtom declStore adtMap (IR.Atom pred terms) =
  case coerce pred `M.lookup` declStore of
    Just _ -> A.Atom pred [decompileTerm adtMap $ head terms]
    Nothing -> A.Atom pred (decompileTerm adtMap <$> terms)

decompileTerm :: ADTMap -> IR.Term -> A.Term
decompileTerm _ (IR.Var (IR.Variable var n)) = A.Var (A.Variable (var <> T.pack (show n)))
decompileTerm _ (IR.Sym sym) = A.Sym (coerce sym)
decompileTerm adtMap (IR.Id id) =
  case id `M.lookup` adtMap of
    Just t -> t
    Nothing -> error
             $ "ICE: there is no corresponding ADT for dentifier " <> show id
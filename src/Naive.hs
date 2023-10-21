{-# LANGUAGE OverloadedStrings #-}

module Naive (drive) where

import           Prelude hiding (head)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           IntermediateRepresentation
import qualified Data.Equivalence.Monad as EM
import           Data.Traversable (forM)
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Control.Monad.Trans.State.Strict as St
import           Control.Monad (foldM)
import           Control.Monad.Trans.Class (lift)
import qualified KnowledgeBase as KB

type KnowledgeBase = KB.KnowledgeBase Atom
type KnowledgeBaseInternal = KB.KnowledgeBaseInternal Atom

type Substitution = M.Map Term Term

drive :: Program -> KnowledgeBase
drive prog = KB.KnowledgeBase $ go S.empty
  where
    go kb = let kb' = step kb prog
            in if kb == kb'
               then kb
               else go kb'

step :: KnowledgeBaseInternal -> Program -> KnowledgeBaseInternal
step kb clauses = S.unions $ kb:(stepClause kb <$> clauses)

stepClause :: KnowledgeBaseInternal -> Clause -> KnowledgeBaseInternal
stepClause kb (Clause head body) =
  S.map (normaliseFact . applySubst head) substs
  where
    substs = evalFreshM $ foldM (evalAtom kb) (S.singleton M.empty) body

evalAtom
  :: KnowledgeBaseInternal -> S.Set Substitution -> Atom -> FreshM (S.Set Substitution)
evalAtom kb substs atom = S.fromList <$> foldM considerSubst [] substs
  where
    considerSubst acc subst = do
      -- Specialise the atom before doing unification, this is not a must but
      -- database literature shows that it is more performant in general. In
      -- our case, it'll likely means fewer things will need to be unified.
      let atom' = applySubst atom subst
      substs' <- foldM (considerFact atom') [] kb
      -- The resulting substitutions are an extension to the given.
      let substs'' = compSubst subst <$> substs'
      pure $ substs'' <> acc

    considerFact atom' acc fact = do
      substMaybe <- unify atom' fact
      pure $ maybe acc (: acc) substMaybe

-- p(X,X) ~ p(42,43) = Nothing
-- p(X,X) ~ p(42,42) = Just [42/X]
-- p(X,X) ~ forall a. p(a,a) = Just []
-- p(X,X) ~ forall a. p(a,b) = Just []
-- p(X,Y,X) ~ forall a. p(a, a, 42) = Just [42/X, 42/Y]
-- p(X,Y,X) ~ forall a b. p(a, a, b) = Just [Y/X]
unify :: Atom -> Atom -> FreshM (Maybe Substitution)
unify (Atom predicate terms) (Atom predicate' terms')
  | predicate /= predicate' = pure Nothing
  | otherwise = do
    -- Get around variable capture by specialising variables
    terms'' <- specialiseTerms terms'
    pure
      $ EM.runEquivM id min
      $ do
        unifies <- and <$> traverse unifyTerm (zip terms terms'')
        if unifies
          then equivalenceToSubst
          else pure Nothing
  where
    equivalenceToSubst :: EM.EquivM s Term Term (Maybe Substitution)
    equivalenceToSubst = do
      ts <- EM.values
      bindings <- forM ts
        $ \term -> case term of
          Var {} -> do
            desc <- EM.classDesc term
            -- Skip the identity binding, e.g., [X/X]
            if term == desc
              then pure Nothing
              else pure $ Just (term, desc)
          Sym _  ->
            -- No substitution from constants
            pure Nothing
          Id _   ->
            -- No substitution from identifiers
            pure Nothing
      pure $ Just $ M.fromList $ catMaybes bindings

    unifyTerm :: (Term, Term) -> EM.EquivM s Term Term Bool
    unifyTerm (t1, t2) = do
      t1' <- EM.classDesc t1
      t2' <- EM.classDesc t2
      case (t1', t2') of
        (Sym sym, Sym sym') -> pure $ sym == sym'
        _ -> EM.equate t1 t2 >> pure True

compSubst :: Substitution -> Substitution -> Substitution
compSubst subst1 subst2 =
  M.unionWith const (M.map (applySubstTerm subst2) subst1) subst2

applySubst :: Atom -> Substitution -> Atom
applySubst (Atom predicate terms) subst =
  Atom predicate (applySubstTerm subst <$> terms)

applySubstTerm :: Substitution -> Term -> Term
applySubstTerm subst t = fromMaybe t (M.lookup t subst)

type FreshM a = St.State Int a

evalFreshM :: FreshM a -> a
evalFreshM action = St.evalState action 0

-- Monadic specialisation of terms. For example, if we have `forall X,Y .
-- p(X,Y,X)`. This becomes `p(<X,0>, <Y,1>, <X,0>)`. The counter is every
-- increasing so a subsequent call to specialisation over the same fact, would
-- produce `p(<X,2>, <Y,3>, X<2>)`.
specialiseTerms :: [Term] -> FreshM [Term]
specialiseTerms terms = St.evalStateT (traverse go terms) M.empty
  where
    go t@Sym {} = pure t
    go t@Id {} = pure t
    go (Var (Variable name n)) = do
      mapping <- St.get
      case (name, n) `M.lookup` mapping of
        Just n'  -> pure $ Var (Variable name n')
        Nothing -> do
          ctr <- lift St.get
          lift $ St.put $ ctr + 1
          St.put (M.insert (name, n) ctr mapping)
          pure $ Var (Variable name ctr)

-- We do capture avoiding substitution through renaming and syntactically allow
-- different binders to be used, so, `forall a b. p(a, b)` can have many
-- different representations. This both leads to redundant facts and might lead
-- to non-termiantion due to our counter-based fresh name generation scheme.
--
-- This normalisation removes the string part of variables and reduces the
-- ingeger part of variables to natural numbers starting at 0. This means the
-- first quantified variable will always be `("", 0)`, the second one `("", 1)`,
-- and so on.
--
-- Example: both `p(X,Y,X)` and `p(Z,X,Z)` will be represented as `p(0,1,0)`.
normaliseFact :: Atom -> Atom
normaliseFact (Atom predicate terms) = Atom predicate simplifiedTerms
  where
    simplifiedTerms = St.evalState (traverse simplifyTerm terms) (M.empty, 0)

    simplifyTerm t@Sym {} = pure t
    simplifyTerm t@Id {} = pure t
    simplifyTerm (Var (Variable name n)) = do
      (mapping, ctr) <- St.get
      n' <- case (name, n) `M.lookup` mapping of
        Just n' -> pure n'
        Nothing -> do
          St.put (M.insert (name, n) ctr mapping, ctr + 1)
          pure ctr
      pure $ Var (Variable "" n')
module AST
    ( Program
    , Clause(..)
    , Atom(..)
    , Predicate(..)
    , Term(..)
    , Variable(..)
    , Symbol(..)) where

import qualified Data.Text as T

type Program = [Clause]

data Clause = Clause Atom [Atom]

data Atom = Atom Predicate [Term]
  deriving (Eq, Ord)

newtype Predicate = Predicate T.Text
  deriving (Eq, Ord)

data Term = Sym Symbol
          | Var Variable
  deriving (Eq, Ord)

data Variable = Variable T.Text Int
  deriving (Eq, Ord)

newtype Symbol = Symbol T.Text
  deriving (Eq, Ord)
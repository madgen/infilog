{-# LANGUAGE FlexibleInstances #-}
module AST
    ( Program
    , Entity(..)
    , Declaration(..)
    , Ty(..)
    , Clause(..)
    , Atom(..)
    , IR.Predicate(..)
    , IR.Symbol(..)
    , Variable(..)
    , Constructor(..)
    , Term(..)) where
    
import Prelude hiding (pred, head)
import qualified Data.Text as T
import Data.List (intercalate)
import qualified IntermediateRepresentation as IR

type Program = [Entity]

data Entity = EClause Clause | EDeclaration Declaration

data Declaration = Declaration Ty [(Constructor, [Ty])]

data Ty = TySymbol | TyComposite Constructor

data Clause = Clause Atom [Atom]

data Atom = Atom IR.Predicate [Term]
  deriving (Eq, Ord)

data Term = Sym IR.Symbol
          | Var Variable
          | Composite Constructor [ Term ]
  deriving (Eq, Ord)

newtype Variable = Variable T.Text
  deriving (Eq, Ord)
  
newtype Constructor = Constructor T.Text
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable var) = T.unpack var

instance Show Constructor where
  show (Constructor cstr) = T.unpack cstr
  
instance Show Term where
  show (Sym sym) = show sym
  show (Var var) = show var
  show (Composite cstr terms) = show cstr <> "(" <> intercalate ", " (show <$> terms) <> ")"
  
instance Show Atom where
  show (Atom pred terms) = show pred <> "(" <> intercalate ", " (show <$> terms) <> ")"
  
instance Show Clause where
  show (Clause head []) = show head <> "."
  show (Clause head body) = show head <> " :- " <> intercalate ", " (show <$> body) <> "."
  
instance Show Ty where
  show TySymbol = "Symbol"
  show (TyComposite cstr) = show cstr

instance Show Declaration where
  show (Declaration ty cstrs) = "type " <> show ty <> " = " <> intercalate " | " (show <$> cstrs)
  
instance Show Entity where
  show (EDeclaration decl) = show decl
  show (EClause clause) = show clause

instance {-# OVERLAPPING #-} Show Program where
  show clauses = intercalate "\n" $ show <$> clauses
{-# LANGUAGE FlexibleInstances #-}
module AST
    ( Program
    , Entity(..)
    , Declaration(..)
    , ConstructorDeclaration(..)
    , Ty(..)
    , TyName(..)
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

data Declaration = Declaration TyName [ConstructorDeclaration]

data ConstructorDeclaration = ConstructorDeclaration Constructor [Ty]

data Ty = TySymbol | TyComposite TyName
  deriving (Eq, Ord)

newtype TyName = TyName T.Text
  deriving (Eq, Ord)

data Clause = Clause Atom [Atom]

data Atom = Atom IR.Predicate [Term]
  deriving (Eq, Ord)

data Term = Sym IR.Symbol
          | Var Variable
          | Composite TyName Constructor [ Term ]
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
  show (Composite ty cstr []) = show ty <> "." <> show cstr
  show (Composite ty cstr terms) = show ty <> "." <> show cstr <> "(" <> intercalate ", " (show <$> terms) <> ")"
  
instance Show Atom where
  show (Atom pred terms) = show pred <> "(" <> intercalate ", " (show <$> terms) <> ")"
  
instance Show Clause where
  show (Clause head []) = show head <> "."
  show (Clause head body) = show head <> " :- " <> intercalate ", " (show <$> body) <> "."
  
instance Show TyName where
  show (TyName ty) = T.unpack ty

instance Show Ty where
  show TySymbol = "Symbol"
  show (TyComposite tyName) = show tyName

instance Show ConstructorDeclaration where
  show (ConstructorDeclaration cstr []) = show cstr
  show (ConstructorDeclaration cstr tys) = show cstr <> " of " <> intercalate " * " (show <$> tys)

instance Show Declaration where
  show (Declaration ty cstrs) = "type " <> show ty <> " = " <> intercalate " | " (show <$> cstrs)
  
instance Show Entity where
  show (EDeclaration decl) = show decl
  show (EClause clause) = show clause

instance {-# OVERLAPPING #-} Show Program where
  show clauses = intercalate "\n" $ show <$> clauses
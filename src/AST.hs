module AST
    ( Program
    , Clause(..)
    , Atom(..)
    , Predicate(..)
    , Term(..)
    , Variable(..)
    , Symbol(..)) where
    
import Prelude hiding (pred)
import qualified Data.Text as T
import Data.List (intercalate)

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
  
instance Show Symbol where
  show (Symbol sym) = "\"" <> T.unpack sym <> "\""

instance Show Variable where
  show (Variable var n) = T.unpack var <> show n
  
instance Show Term where
  show (Var var) = show var
  show (Sym sym) = show sym
  
instance Show Predicate where
  show (Predicate pred) = T.unpack pred

instance Show Atom where
  show (Atom pred terms) = show pred <> "(" <> intercalate ", " (show <$> terms) <> ")"
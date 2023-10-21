module KnowledgeBase (KnowledgeBase(..), KnowledgeBaseInternal, map) where

import Prelude hiding (map)
import Data.List (intercalate)

import qualified Data.Set as S

-- Atoms in the knowledge base might include variables. These are implicitly
-- universally quantified.
newtype KnowledgeBase a = KnowledgeBase (KnowledgeBaseInternal a)
type KnowledgeBaseInternal a = S.Set a

map :: Ord b => (a -> b) -> KnowledgeBase a -> KnowledgeBase b
map f (KnowledgeBase s) = KnowledgeBase $ S.map f s

instance Show a => Show (KnowledgeBase a) where
  show (KnowledgeBase kb) = intercalate "\n" $ show <$> S.toList kb
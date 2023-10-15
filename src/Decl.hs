{-# LANGUAGE LambdaCase #-}
module Decl (declare) where

import qualified Data.Map as M
import AST
import Control.Monad (foldM, unless)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Foldable (traverse_)

type ConstructorStore = M.Map Constructor [Ty]
type DeclStore = M.Map Ty ConstructorStore

declare :: Program -> Either String DeclStore
declare entities = do
  let decls = mapMaybe (\case { EClause _ -> Nothing; EDeclaration decl -> Just decl }) entities
  let allowedTys = S.fromList $ TySymbol : ((\(Declaration ty _) -> ty) <$> decls)
  foldM (declareTy allowedTys) M.empty decls

declareTy :: S.Set Ty -> DeclStore -> Declaration -> Either String DeclStore
declareTy declaredTys store (Declaration ty cstrs) = do
  -- Check that we're not redeclaring a type within a program.
  case ty `M.lookup` store of
    Just _ -> Left $ "The type `" <> show ty <> "` is already declared"
    Nothing -> do
      cstrStore <- foldM (declareConstructor declaredTys) M.empty cstrs
      pure $ M.insert ty cstrStore store

declareConstructor :: S.Set Ty -> ConstructorStore -> ConstructorDeclaration -> Either String ConstructorStore
declareConstructor declaredTys cstrStore (ConstructorDeclaration cstr tys) =
  -- Check that we're not redeclaring a constructor within a given type declaration.
  case cstr `M.lookup` cstrStore of
    Just _ -> Left $ "The constructor `" <> show cstr <> "` is already declared"
    Nothing -> do
      -- Check that the types used as parameter exist.
      traverse_ (\ty -> unless (S.member ty declaredTys) (Left $ "The type `" <> show ty <> "` doesn't exist.")) tys
      pure $ M.insert cstr tys cstrStore
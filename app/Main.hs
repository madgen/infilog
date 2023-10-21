module Main (main) where

import           Prelude hiding (lex)
import           Lexer (lex)
import           Parser (parse)
import           Decl (declare)
import qualified Naive
import           Compiler (compile)
import           Decompiler (decompileAtom)
import qualified KnowledgeBase as KB
import qualified Data.Text.IO as T

main :: IO ()
main = do
  contents <- T.getContents
  let ast = parse . lex $ contents
  case declare ast of
    Left err -> error err
    Right decls -> do
      let (ir, adtMap) = compile decls ast
      print $ KB.map (decompileAtom decls adtMap) $ Naive.drive ir

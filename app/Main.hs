module Main (main) where

import           Prelude hiding (lex)
import           Lexer (lex)
import           Parser (parse)
import qualified Naive
import           Compiler (compile)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  contents <- T.getContents
  let ast = parse . lex $ contents
  let ir = compile ast
  print $ Naive.drive ir

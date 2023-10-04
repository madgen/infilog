module Main (main) where

import           Prelude hiding (lex)
import           Lexer (lex)
import           Parser (parse)
import qualified Naive
import qualified Data.Text.IO as T

main :: IO ()
main = do
  contents <- T.getContents
  let ast = parse . lex $ contents
  print $ Naive.drive ast

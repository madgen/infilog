{
{-# LANGUAGE RecordWildCards #-}
module Lexer (lex, Token(..)) where

import qualified Data.Text as T

import Prelude hiding (lex)
}

%wrapper "strict-text"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                        ;
  \%.*                           ;
  \".*\"                         { \s -> TSymbol (stripSymbolLiteral s) }
  :\-                            { \_ -> TImplication }
  \.                             { \_ -> TDot }
  \,                             { \_ -> TComma }
  \(                             { \_ -> TLeftPar }
  \)                             { \_ -> TRightPar }
  $alpha [$alpha $digit \_ \']*  { \s -> TId s }

{
data Token =
    TLeftPar
  | TRightPar
  | TImplication
  | TDot
  | TComma
  | TSymbol T.Text
  | TId T.Text
  deriving (Eq, Show)
  
stripSymbolLiteral :: T.Text -> T.Text
stripSymbolLiteral sym = T.take (T.length sym - 2) . T.drop 1 $ sym

lex = alexScanTokens
}
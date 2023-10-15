{
{-# LANGUAGE OverloadedStrings #-}
module Parser (parse) where

import AST
import Lexer
}
%name parse1
%tokentype { Token }
%error { parseError }

%token
      ':-'            { TImplication }
      '('             { TLeftPar }
      ')'             { TRightPar }
      '.'             { TDot }
      ','             { TComma }
      '|'             { TBar }
      '*'             { TStar }
      '='             { TEqual }
      of              { TOf }
      type            { TType }
      sym             { TSymbol $$ }
      id              { TId $$ }
      
%%

Program :: { Program }
Program : Entity                             { [ $1 ] }
        | Program Entity                     { $2 : $1 }
        
Entity :: { Entity }
Entity : Clause                              { EClause $1 }
       | Declaration                         { EDeclaration $1 }
       
Declaration :: { Declaration }
Declaration : type Type '=' Constructors     { Declaration $2 (reverse $4) }

Constructors :: { [(Constructor, [Ty])] }
Constructors : Constructor                   { [ $1 ] }
             | Constructors '|' Constructor  { $3 : $1 }
             
Constructor :: { (Constructor, [Ty]) }
Constructor : id of Types                    { (Constructor $1, reverse $3) }
Constructor : id                             { (Constructor $1, []) }
            
Types :: { [Ty] }
Types : Type                                 { [ $1 ] }
      | Types '*' Type                       { $3 : $1 }
      
Type :: { Ty }
Type : id                                    { if $1 == "Symbol" then TySymbol else TyComposite (TyName $1) }

Clause :: { Clause }
Clause : Atom '.'                            { Clause $1 [] }
       | Atom ':-' Body '.'                  { Clause $1 (reverse $3) }

Body :: { [ Atom ] }
Body : Atom                                  { [ $1 ] }
     | Body ',' Atom                         { $3 : $1 }

Atom :: { Atom }
Atom : id '(' ')'                            { Atom (Predicate $1) [] }
     | id '(' Terms ')'                      { Atom (Predicate $1) (reverse $3) }
    
Terms :: { [ Term ] }
Terms : Term                                 { [ $1 ] }
      | Terms ',' Term                       { $3 : $1 }
      
Term :: { Term }
Term : sym                                   { Sym $ Symbol $1 }
     | id                                    { Var $ Variable $1 }
     | id '.' id '(' Terms ')'               { Composite (TyComposite $ TyName $1) (Constructor $3) $5 }
{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error, here are the tokens:\n" <> show tokens

parse = reverse . parse1
}
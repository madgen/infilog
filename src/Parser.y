{
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
      sym             { TSymbol $$ }
      id              { TId $$ }
      
%%

Program :: { Program }
Program : Clause            { [ $1 ] }
        | Program Clause    { $2 : $1 }

Clause :: { Clause }
Clause : Atom '.'           { Clause $1 [] }
       | Atom ':-' Body '.' { Clause $1 (reverse $3) }

Body :: { [ Atom ] }
Body : Atom                 { [ $1 ] }
     | Body ',' Atom        { $3 : $1 }

Atom :: { Atom }
Atom : id '(' ')'           { Atom (Predicate $1) [] }
     | id '(' Terms ')'     { Atom (Predicate $1) (reverse $3) }
    
Terms :: { [ Term ] }
Terms : Term                { [ $1 ] }
      | Terms ',' Term      { $3 : $1 }
      
Term :: { Term }
Term : sym                  { Sym $ Symbol $ $1 }
     | id                   { Var $ Variable $1 0 }
{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error, here are the tokens:\n" <> show tokens

parse = reverse . parse1
}
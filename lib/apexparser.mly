/* File parser.mly */
%{
open Ast
%}

%token <int> INT
%token PLUS
%token MINUS
%token MULT
%token DIV
%token ABSTRACT
%token AFTER
%token EOL
%type <Ast.expr> main
%start main

%%
expr:  i = INT { Int i }

main: 
    e = expr; {e}
;

%%
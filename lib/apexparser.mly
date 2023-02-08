/* File parser.mly */
%{
open Ast
%}

%token <int> INT
%token PLUS
%token MULT
%token ABSTRACT
%token AFTER
%token EOF
%type <Ast.expr> main
%start main

%%
expr:  
    | i = INT { Int i } 
    | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2)} 
    | e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2)} 
;

main: 
    e = expr; EOF {e}
;

%%
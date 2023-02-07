/* File parser.mly */
%token <int> INT
%token PLUS
%token MINUS
%token MULT
%token DIV
%token ABSTRACT
%token EOL
%start main
%type <int> main

%%
main: 
    expr EOL {$1}
;

expr:   INT {$1};
%%
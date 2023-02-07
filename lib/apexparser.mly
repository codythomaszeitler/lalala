/* File parser.mly */
%token <int> INT
%token PLUS
%token MINUS
%token MULT
%token EOL
%start main
%type <int> main

%%
main: 
    expr EOL {$1}
;

expr:   INT {$1};
%%
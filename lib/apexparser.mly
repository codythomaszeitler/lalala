/* File parser.mly */
%{
open Ast
%}

%token <int> INT
%token PLUS
%token MULT
%token ABSTRACT
%token AFTER
%token PUBLIC
%token CLASS
%token LEFT_BRACE 
%token <string> ID
%token RIGHT_BRACE
%token EOF

// So.. there should be something that could 
// be like... the top level of aynthing. 

%type <Ast.compilationUnit> compilationUnit
%start compilationUnit

%%
// expr:  
//     | i = INT { Int i } 
//     | e1 = expr; PLUS; e2 = expr { Expr(Binop (Add, e1, e2))} 
//     | e1 = expr; MULT; e2 = expr { Expr(Binop (Mult, e1, e2))} 
// ;

compilationUnit: 
    | decl = typeDeclaration {decl}
;

typeDeclaration :
    | modi = modifier; decl = classDeclaration {TypeDecl(Location, modi, decl)}
;

modifier:
    | PUBLIC {Public(Location)}
;

identifier:
    | id = ID {Identifier(Location, id)}
;

// Well what exactly are we going to return here. 
classDeclaration:
    | CLASS; id = identifier; LEFT_BRACE; RIGHT_BRACE {ClassDeclaration(Location, id)}
;

%%
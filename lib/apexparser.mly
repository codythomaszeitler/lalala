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
%token SEMI
%token COMMA
%token ASSIGN
%token STATIC
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
    | CLASS; id = identifier; body = classBody {ClassDeclaration(Location, id, body)}
;

classBody:
    | LEFT_BRACE; decls = classBodyDeclaration*; RIGHT_BRACE {decls}
;

classBodyDeclaration:
    | modi = modifier membDecl = memberDeclaration {ClassBodyDeclaration(Location, modi, membDecl)}
;

memberDeclaration:
    | decl = fieldDeclaration {decl}

fieldDeclaration
    : apexType = typeRef decls = variableDeclarators SEMI {FieldDeclaration(Location, apexType, decls)}
    ;

variableDeclarators
    : decls = separated_nonempty_list(COMMA, variableDeclarator) {decls} 
    ;

variableDeclarator
    : iden = id {VariableDecl(Location, iden)} //(ASSIGN expression)?
    ;

typeRef:
    | apexType = typeName {apexType} // (DOT typeName)* arraySubscripts
;

typeName:
    | idd = id {ApexType(Location, idd)}
;

id:  
    | iden = ID {Identifier(Location, iden)}
;

// block :
//     | LEFT_BRACE stmts = statement* RIGHT_BRACE {stmts}
// ;

// statement:
//     | localVarDeclStmt = localVariableDeclarationStatement {localVarDeclStmt}
// ;

// localVariableDeclarationStatement
//     : localVarDecl = localVariableDeclaration SEMI {LocalVarDeclStmt(Location, localVarDecl)}
//     ;

// localVariableDeclaration
//     : modi = modifier apexType = typeRef decls = variableDeclarators {LocalVarDecl(Location, modi, apexType, decls)}
//     ;

// memberDeclaration
//     : methodDeclaration
//     | fieldDeclaration
//     ;

%%
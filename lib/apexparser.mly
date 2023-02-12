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
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMI
%token RETURN
%token COMMA
%token ASSIGN
%token STATIC
%token <string> ID
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
    | decl = methodDeclaration {decl}
;

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

methodDeclaration
    : apexType = typeRef id = id LEFT_PAREN RIGHT_PAREN stmts = block  {MethodDeclaration(Location, apexType, id, stmts)}
;

block :
    | LEFT_BRACE stmts = statement* RIGHT_BRACE {stmts}
;

statement:
    | localVarDeclStmt = localVariableDeclarationStatement {localVarDeclStmt}
    | returnStmt = returnStatement {returnStmt}
;

localVariableDeclarationStatement
    : localVarDecl = localVariableDeclaration SEMI {LocalVarDeclStmt(Location, localVarDecl)}
;

localVariableDeclaration
    : modi = modifier apexType = typeRef decls = variableDeclarators {LocalVarDecl(Location, modi, apexType, decls)}
;

returnStatement
    : RETURN expr = expression SEMI {ReturnStmt(Location, expr)}
;

expressionStatement
    : expr = expression SEMI {expr}
;

primary
    : 
    | id = id {Primary(Location, id)}
;

expression
    : expr = primary {expr}

// memberDeclaration
//     : methodDeclaration
//     | fieldDeclaration
//     ;

%%
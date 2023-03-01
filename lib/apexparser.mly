/* File parser.mly */
%{
open Ast
open Location
%}

%token <int> INT
%token PLUS
%token MULT
%token ABSTRACT
%token AFTER
%token PUBLIC
%token PRIVATE
%token CLASS
%token LEFT_BRACE 
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMI
%token RETURN
%token COMMA
%token ASSIGN
%token ATSIGN
%token STATIC
%token <string> ID
%token EOF


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
    |  decl = classDeclaration {decl}
;

modifier:
    | PUBLIC {ApexModifier.Public(no_loc)}
    | PRIVATE {ApexModifier.Private(no_loc)}
;

identifier:
    | id = ID {ApexIdentifier.ApexIdentifier(no_loc, id)}
;

classDeclaration:
    | anno=annotation? modis = modifier*; CLASS; id = identifier; body = classBody {ApexClassDeclaration(no_loc, anno, modis, id, body)}
;

classBody:
    | LEFT_BRACE; decls = classBodyDeclaration*; RIGHT_BRACE {decls}
;

classBodyDeclaration:
    | decl = memberDeclaration {decl}
;

memberDeclaration:
    | decl = fieldDeclaration {decl}
    | decl = methodDeclaration {decl}
;

fieldDeclaration
    : anno=annotation? modis=modifier* apexType = typeRef decls = variableDeclarators SEMI {ApexDecl.ApexFieldDeclaration(no_loc, anno, modis, apexType, decls)}
    ;

variableDeclarators
    : decls = separated_nonempty_list(COMMA, variableDeclarator) {decls} 
    ;

variableDeclarator
    : iden = id {ApexVariableDecl.ApexVariableDecl(no_loc, iden)} //(ASSIGN expression)?
    ;

typeRef:
    | apexType = typeName {apexType} // (DOT typeName)* arraySubscripts
;

typeName:
    | idd = id { match idd with ApexIdentifier.ApexIdentifier(_, name) ->  ApexType.ApexType(no_loc, name)}
;

id:  
    | iden = ID {ApexIdentifier.ApexIdentifier(no_loc, iden)}
;

methodDeclaration
    : anno=annotation? modis=modifier* apexType = typeRef id = id LEFT_PAREN RIGHT_PAREN stmts = block  {ApexDecl.ApexMethodDeclaration(no_loc, anno, modis, apexType, id, stmts)}
;

block :
    | LEFT_BRACE stmts = statement* RIGHT_BRACE {stmts}
;

statement:
    | localVarDeclStmt = localVariableDeclarationStatement {localVarDeclStmt}
    | returnStmt = returnStatement {returnStmt}
;

localVariableDeclarationStatement
    : localVarDecl = localVariableDeclaration SEMI {Stmt.ApexLocalVarDeclStmt(no_loc,localVarDecl)}
;

localVariableDeclaration
    : modi = modifier apexType = typeRef decls = variableDeclarators {ApexLocalVarDecl.ApexLocalVarDecl(no_loc, modi, apexType, decls)}
;

returnStatement
    : RETURN expr = expression SEMI {Stmt.ApexReturnStmt(no_loc, expr)}
;

expressionStatement
    : expr = expression SEMI {expr}
;

primary
    : 
    | id = id {Expr.Id(no_loc, id)}
;

expression
    : expr = primary {expr}
;

annotation
    : ATSIGN name = ID {ApexAnnotation.ApexAnnotation(no_loc, name)}
;

qualifiedName
    : id = id {id}
;


%%
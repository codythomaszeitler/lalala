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
%token ATSIGN
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
    | modis = modifier*; decl = classDeclaration {TypeDecl(modis, decl)}
;

modifier:
    | PUBLIC {Public}
    | anno = annotation {anno}
;

identifier:
    | id = ID {Identifier.Identifier(id)}
;

classDeclaration:
    | CLASS; id = identifier; body = classBody {ClassDeclaration(id, body)}
;

classBody:
    | LEFT_BRACE; decls = classBodyDeclaration*; RIGHT_BRACE {decls}
;

classBodyDeclaration:
    | modi = modifier membDecl = memberDeclaration {ClassBodyDeclaration(modi, membDecl)}
;

memberDeclaration:
    | decl = fieldDeclaration {decl}
    | decl = methodDeclaration {decl}
;

fieldDeclaration
    : apexType = typeRef decls = variableDeclarators SEMI {FieldDeclaration(apexType, decls)}
    ;

variableDeclarators
    : decls = separated_nonempty_list(COMMA, variableDeclarator) {decls} 
    ;

variableDeclarator
    : iden = id {VariableDecl(iden)} //(ASSIGN expression)?
    ;

typeRef:
    | apexType = typeName {apexType} // (DOT typeName)* arraySubscripts
;

typeName:
    | idd = id {ApexType(idd)}
;

id:  
    | iden = ID {Identifier(iden)}
;

methodDeclaration
    : apexType = typeRef id = id LEFT_PAREN RIGHT_PAREN stmts = block  {MethodDeclaration(apexType, id, stmts)}
;

block :
    | LEFT_BRACE stmts = statement* RIGHT_BRACE {stmts}
;

statement:
    | localVarDeclStmt = localVariableDeclarationStatement {localVarDeclStmt}
    | returnStmt = returnStatement {returnStmt}
;

localVariableDeclarationStatement
    : localVarDecl = localVariableDeclaration SEMI {LocalVarDeclStmt(localVarDecl)}
;

localVariableDeclaration
    : modi = modifier apexType = typeRef decls = variableDeclarators {LocalVarDecl(modi, apexType, decls)}
;

returnStatement
    : RETURN expr = expression SEMI {ReturnStmt(expr)}
;

expressionStatement
    : expr = expression SEMI {expr}
;

primary
    : 
    | id = id {Primary(id)}
;

expression
    : expr = primary {expr}
;

annotation
    : ATSIGN name = qualifiedName {Annotation(name)}
;

qualifiedName
    : id = id {id}
;

// memberDeclaration
//     : methodDeclaration
//     | fieldDeclaration
//     ;

%%
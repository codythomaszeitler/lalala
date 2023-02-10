(* type bop = Add | Mult | Leq

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr *)

type loc = Location
type modifier = Public of loc
type identifier = Identifier of loc * string



type apexType = ApexType of loc * identifier

type variableDecl = VariableDecl of loc * identifier
type memberDeclaration = FieldDeclaration of loc * apexType * variableDecl list
type localVarDecl = LocalVarDecl of loc * modifier * apexType * variableDecl list
type statement = LocalVarDeclStmt of loc * localVarDecl
type classBodyDeclaration = ClassBodyDeclaration of loc * modifier * memberDeclaration 
type classDeclaration = ClassDeclaration of loc * identifier * classBodyDeclaration list

type compilationUnit =
  | TypeDecl of loc * modifier * classDeclaration
  (* | Expr of expr *)

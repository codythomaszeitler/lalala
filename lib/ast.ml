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
type classDeclaration = ClassDeclaration of loc * identifier
type compilationUnit =
  | TypeDecl of loc * modifier * classDeclaration
  (* | Expr of expr *)

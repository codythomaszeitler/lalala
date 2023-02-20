type expr = Primary of Identifier.node

module Expr = struct
  let to_string (expr : expr) (depth : int) =
    match expr with
    | Primary identifier ->
        "(Primary\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ "))"
end

type modifier = Public | Annotation of Identifier.node

module Modifier = struct
  let to_string (modifier : modifier) (depth : int) =
    match modifier with
    | Public -> "(Public)"
    | Annotation identifer ->
        "(Annotation\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifer (depth + 1)
        ^ ")"

  let rec _to_strings (modifiers : modifier list) (depth : int) =
    match modifiers with
    | [] -> ""
    | h :: [] -> Formatter.tabs depth ^ to_string h depth
    | h :: t -> Formatter.tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

  let to_strings (modifiers : modifier list) (depth : int) =
    "[\n" ^ _to_strings modifiers (depth + 1) ^ "\n" ^ Formatter.tabs depth ^ "]"
end

type apexType = ApexType of Identifier.node

module ApexType = struct
  let to_string (apex_type : apexType) (depth : int) =
    match apex_type with
    | ApexType identifier ->
        "(ApexType\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ ")"
end

type variableDecl = VariableDecl of Identifier.node
type localVarDecl = LocalVarDecl of modifier * apexType * variableDecl list
type statement = LocalVarDeclStmt of localVarDecl | ReturnStmt of expr

module Statement = struct
  let to_string (stmt : statement) (depth : int) =
    match stmt with
    | ReturnStmt expr ->
        "(ReturnStmt\n"
        ^ Formatter.tabs (depth + 1)
        ^ Expr.to_string expr (depth + 1)
        ^ ")"
    | _ -> "Not supported yet"

  let rec _to_strings (stmts : statement list) (depth : int) =
    match stmts with
    | [] -> ""
    | h :: [] -> Formatter.tabs depth ^ to_string h depth
    | h :: t -> Formatter.tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

  let to_strings (stmts : statement list) (depth : int) =
    "[\n" ^ _to_strings stmts (depth + 1) ^ "]"
end

type memberDeclaration =
  | FieldDeclaration of apexType * variableDecl list
  | MethodDeclaration of apexType * Identifier.node * statement list

module MemberDeclaration = struct
  let to_string (member_decl : memberDeclaration) (depth : int) =
    match member_decl with
    | MethodDeclaration (apexType, identifier, stmts) ->
        Formatter.tabs depth ^ "(MethodDeclaration\n"
        ^ Formatter.tabs (depth + 1)
        ^ ApexType.to_string apexType (depth + 1)
        ^ ",\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ ",\n"
        ^ Formatter.tabs (depth + 1)
        ^ Statement.to_strings stmts (depth + 1)
        ^ ")"
    | _ -> "Not implemented yet!"
end

type classBodyDeclaration =
  | ClassBodyDeclaration of modifier * memberDeclaration

module ClassBodyDeclaration = struct
  let to_string (class_body_decl : classBodyDeclaration) (depth : int) =
    match class_body_decl with
    | ClassBodyDeclaration (modifier, method_decl) ->
        "(ClassBodyDeclaration\n"
        ^ Formatter.tabs (depth + 1)
        ^ Modifier.to_string modifier (depth + 1)
        ^ ",\n" ^ Formatter.tabs depth
        ^ MemberDeclaration.to_string method_decl (depth + 1)
        ^ ")"

  let to_strings (class_body_decls : classBodyDeclaration list) (depth : int) =
    match class_body_decls with
    | [] -> Formatter.tabs depth ^ "[]"
    | _ -> "Not implemented with class body decls to string"
end

type classDeclaration =
  | ClassDeclaration of Identifier.node * classBodyDeclaration list

module ClassDeclaration = struct
  let to_string (class_decl : classDeclaration) (depth : int) =
    match class_decl with
    | ClassDeclaration (identifer, class_body_decls) ->
        Formatter.tabs depth ^ "(ClassDeclaration\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifer (depth + 1)
        ^ ",\n" ^ Formatter.tabs depth
        ^ ClassBodyDeclaration.to_strings class_body_decls depth
        ^ "))"
end

type compilationUnit = TypeDecl of modifier list * classDeclaration

module CompilationUnit = struct
  let to_string (compilationUnit : compilationUnit) =
    match compilationUnit with
    | TypeDecl (modifiers, classDeclaration) ->
        "(TypeDecl(\n  "
        ^ Modifier.to_strings modifiers 1
        ^ ",\n"
        ^ ClassDeclaration.to_string classDeclaration 1
        ^ ")"
end

(* let print_compilation_unit compilation_unit =
    function
      | TypeDecl typeDecl -> *)

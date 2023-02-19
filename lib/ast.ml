let rec tabs (num : int) = if num == 0 then "" else tabs (num - 1) ^ "  "

type identifier = Identifier of string

module Identifier = struct
  let to_string (identifier : identifier) (depth : int) =
    match identifier with
    | Identifier name ->
        "(Identifier\n" ^ tabs (depth + 1) ^ "(\"" ^ name ^ "\"))"
end

type expr = Primary of identifier

module Expr = struct
  let to_string (expr : expr) (depth : int) =
    match expr with
    | Primary identifier ->
        "(Primary\n"
        ^ tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ "))"
end

type modifier = Public | Annotation of identifier

module Modifier = struct
  let to_string (modifier : modifier) (depth : int) =
    match modifier with
    | Public -> "(Public)"
    | Annotation identifer ->
        "(Annotation\n"
        ^ tabs (depth + 1)
        ^ Identifier.to_string identifer (depth + 1)
        ^ ")"

  let rec _to_strings (modifiers : modifier list) (depth : int) =
    match modifiers with
    | [] -> ""
    | h :: [] -> tabs depth ^ to_string h depth
    | h :: t -> tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

  let to_strings (modifiers : modifier list) (depth : int) =
    "[\n" ^ _to_strings modifiers (depth + 1) ^ "\n" ^ tabs depth ^ "]"
end

type apexType = ApexType of identifier

module ApexType = struct
  let to_string (apex_type : apexType) (depth : int) =
    match apex_type with
    | ApexType identifier ->
        "(ApexType\n"
        ^ tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ ")"
end

type variableDecl = VariableDecl of identifier
type localVarDecl = LocalVarDecl of modifier * apexType * variableDecl list
type statement = LocalVarDeclStmt of localVarDecl | ReturnStmt of expr

module Statement = struct
  let to_string (stmt : statement) (depth : int) =
    match stmt with
    | ReturnStmt expr ->
        "(ReturnStmt\n"
        ^ tabs (depth + 1)
        ^ Expr.to_string expr (depth + 1)
        ^ ")"
    | _ -> "Not supported yet"

  let rec _to_strings (stmts : statement list) (depth : int) =
    match stmts with
    | [] -> ""
    | h :: [] -> tabs depth ^ to_string h depth
    | h :: t -> tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

  let to_strings (stmts : statement list) (depth : int) =
    "[\n" ^ _to_strings stmts (depth + 1) ^ "]"
end

type memberDeclaration =
  | FieldDeclaration of apexType * variableDecl list
  | MethodDeclaration of apexType * identifier * statement list

module MemberDeclaration = struct
  let to_string (member_decl : memberDeclaration) (depth : int) =
    match member_decl with
    | MethodDeclaration (apexType, identifier, stmts) ->
        tabs depth ^ "(MethodDeclaration\n"
        ^ tabs (depth + 1)
        ^ ApexType.to_string apexType (depth + 1)
        ^ ",\n"
        ^ tabs (depth + 1)
        ^ Identifier.to_string identifier (depth + 1)
        ^ ",\n"
        ^ tabs (depth + 1)
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
        ^ tabs (depth + 1)
        ^ Modifier.to_string modifier (depth + 1)
        ^ ",\n" ^ tabs depth
        ^ MemberDeclaration.to_string method_decl (depth + 1)
        ^ ")"

  let to_strings (class_body_decls : classBodyDeclaration list) (depth : int) =
    match class_body_decls with
    | [] -> tabs depth ^ "[]"
    | _ -> "Not implemented with class body decls to string"
end

type classDeclaration =
  | ClassDeclaration of identifier * classBodyDeclaration list

module ClassDeclaration = struct
  let to_string (class_decl : classDeclaration) (depth : int) =
    match class_decl with
    | ClassDeclaration (identifer, class_body_decls) ->
        tabs depth ^ "(ClassDeclaration\n"
        ^ tabs (depth + 1)
        ^ Identifier.to_string identifer (depth + 1)
        ^ ",\n" ^ tabs depth
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

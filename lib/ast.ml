let rec tabs (num : int) = if num == 0 then "" else tabs (num - 1) ^ "  "

type identifier = Identifier of string

module Identifier = struct
  let to_string (identifier : identifier) (depth : int) =
    match identifier with
    | Identifier name ->
        "(Identifier\n" ^ tabs (depth + 1) ^ "(\"" ^ name ^ "\"))"
end

type expr = Primary of identifier
type modifier = Public | Annotation of identifier
type apexType = ApexType of identifier
type variableDecl = VariableDecl of identifier
type localVarDecl = LocalVarDecl of modifier * apexType * variableDecl list
type statement = LocalVarDeclStmt of localVarDecl | ReturnStmt of expr

type memberDeclaration =
  | FieldDeclaration of apexType * variableDecl list
  | MethodDeclaration of apexType * identifier * statement list

type classBodyDeclaration =
  | ClassBodyDeclaration of modifier * memberDeclaration

type classDeclaration =
  | ClassDeclaration of identifier * classBodyDeclaration list

type compilationUnit = TypeDecl of modifier list * classDeclaration

let to_string_expr (expr : expr) (depth : int) =
  match expr with
  | Primary identifier ->
      "(Primary\n"
      ^ tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ "))"

let to_string_statement (stmt : statement) (depth : int) =
  match stmt with
  | ReturnStmt expr ->
      "(ReturnStmt\n" ^ tabs (depth + 1) ^ to_string_expr expr (depth + 1) ^ ")"
  | _ -> "Not supported yet"

let rec _to_string_statements (stmts : statement list) (depth : int) =
  match stmts with
  | [] -> ""
  | h :: [] -> tabs depth ^ to_string_statement h depth
  | h :: t ->
      tabs depth
      ^ to_string_statement h depth
      ^ ";\n"
      ^ _to_string_statements t depth

let to_string_statements (stmts : statement list) (depth : int) =
  "[\n" ^ _to_string_statements stmts (depth + 1) ^ "]"

let to_string_apex_type (apex_type : apexType) (depth : int) =
  match apex_type with
  | ApexType identifier ->
      "(ApexType\n"
      ^ tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ ")"

let to_string_member_decl (member_decl : memberDeclaration) (depth : int) =
  match member_decl with
  | MethodDeclaration (apexType, identifier, stmts) ->
      tabs depth ^ "(MethodDeclaration\n"
      ^ tabs (depth + 1)
      ^ to_string_apex_type apexType (depth + 1)
      ^ ",\n"
      ^ tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ ",\n"
      ^ tabs (depth + 1)
      ^ to_string_statements stmts (depth + 1)
      ^ ")"
  | _ -> "Not implemented yet!"

let to_string_modifier (modifier : modifier) (depth : int) =
  match modifier with
  | Public -> "(Public)"
  | Annotation identifer ->
      "(Annotation\n"
      ^ tabs (depth + 1)
      ^ Identifier.to_string identifer (depth + 1)
      ^ ")"

let rec _to_string_modifiers (modifiers : modifier list) (depth : int) =
  match modifiers with
  | [] -> ""
  | h :: [] -> tabs depth ^ to_string_modifier h depth
  | h :: t ->
      tabs depth ^ to_string_modifier h depth ^ ";\n"
      ^ _to_string_modifiers t depth

let to_string_modifiers (modifiers : modifier list) (depth : int) =
  "[\n" ^ _to_string_modifiers modifiers (depth + 1) ^ "\n" ^ tabs depth ^ "]"

let to_string_class_body_decl (class_body_decl : classBodyDeclaration)
    (depth : int) =
  match class_body_decl with
  | ClassBodyDeclaration (modifier, method_decl) ->
      "(ClassBodyDeclaration\n"
      ^ tabs (depth + 1)
      ^ to_string_modifier modifier (depth + 1)
      ^ ",\n" ^ tabs depth
      ^ to_string_member_decl method_decl (depth + 1)
      ^ ")"

let to_string_class_body_decls (class_body_decls : classBodyDeclaration list)
    (depth : int) =
  match class_body_decls with
  | [] -> tabs depth ^ "[]"
  | _ -> "Not implemented with class body decls to string"

let to_string_class_declaration (class_decl : classDeclaration) (depth : int) =
  match class_decl with
  | ClassDeclaration (identifer, class_body_decls) ->
      tabs depth ^ "(ClassDeclaration\n"
      ^ tabs (depth + 1)
      ^ Identifier.to_string identifer (depth + 1)
      ^ ",\n" ^ tabs depth
      ^ to_string_class_body_decls class_body_decls depth
      ^ "))"

let to_string_compilation_unit (compilationUnit : compilationUnit) =
  match compilationUnit with
  | TypeDecl (modifiers, classDeclaration) ->
      "(TypeDecl(\n  "
      ^ to_string_modifiers modifiers 1
      ^ ",\n"
      ^ to_string_class_declaration classDeclaration 1
      ^ ")"

(* let print_compilation_unit compilation_unit =
    function
      | TypeDecl typeDecl -> *)

type identifier = Identifier of string
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

let rec tabs (num : int) = if num == 0 then "" else tabs (num - 1) ^ "  "

let to_string_identifier (identifier : identifier) (depth : int) =
  match identifier with
  | Identifier name -> "(Identifier\n" ^ tabs (depth + 1) ^ "(\"" ^ name ^ "\"))"

let to_string_modifier (modifier : modifier) (depth : int) =
  match modifier with
  | Public -> "(Public)"
  | Annotation identifer ->
      "(Annotation\n"
      ^ tabs (depth + 1)
      ^ to_string_identifier identifer (depth + 1)
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

let to_string_class_body_decls (class_body_decls : classBodyDeclaration list) (depth : int) =
  match class_body_decls with 
    | [] -> tabs depth ^ "[]"
    | _ -> "Not implemented with class body decls to string"

let to_string_class_declaration (class_decl : classDeclaration) (depth : int) =
  match class_decl with
  | ClassDeclaration (identifer, class_body_decls) ->
      tabs depth ^ "(ClassDeclaration\n"
      ^ tabs (depth + 1)
      ^ to_string_identifier identifer (depth + 1)
      ^ ",\n"
      ^ tabs (depth)
      ^ to_string_class_body_decls class_body_decls (depth)
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

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

let to_string_identifier (identifier : identifier) =
  match identifier with Identifier name -> "Identifier(\"" ^ name ^ "\")"

let to_string_modifier (modifier : modifier) =
  match modifier with
  | Public -> "Public"
  | Annotation identifer -> "Annotation(" ^ to_string_identifier identifer ^ ")"

let rec _to_string_modifiers (modifiers : modifier list) =
  match modifiers with
  | [] -> ""
  | h :: [] -> to_string_modifier h
  | h :: t -> to_string_modifier h ^ ", " ^ _to_string_modifiers t

let to_string_modifiers (modifiers : modifier list) =
  "[" ^ (_to_string_modifiers modifiers) ^ "]"

(* let print_compilation_unit compilation_unit =
    function
      | TypeDecl typeDecl -> *)

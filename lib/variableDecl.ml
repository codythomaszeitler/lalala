type variableDecl = VariableDecl of Identifier.node

let to_string (variable_decl : variableDecl) (depth : int) =
  match variable_decl with
  | VariableDecl identifier ->
      "(VariableDecl\n"
      ^ Formatter.tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ ")"

let to_strings (variable_decls: variableDecl list) (depth: int) =
  let rec _to_strings (variable_decls : variableDecl list) (depth : int) =
    match variable_decls with
    | [] -> ""
    | h :: [] -> to_string h (depth + 1)
    | h :: t ->
        to_string h (depth + 1)
        ^ ";\n"
        ^ Formatter.tabs (depth + 1)
        ^ _to_strings t depth
  in
  "[\n" ^ Formatter.tabs (depth + 1) ^ _to_strings variable_decls depth ^ "]"

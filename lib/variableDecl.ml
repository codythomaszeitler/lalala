type variableDecl = VariableDecl of Identifier.node

let to_string (variable_decl : variableDecl) (depth : int) =
  match variable_decl with
  | VariableDecl identifier ->
      "(VariableDecl\n"
      ^ Formatter.tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ ")"

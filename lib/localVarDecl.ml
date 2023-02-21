type localVarDecl =
  | LocalVarDecl of
      Modifier.node * ApexType.node * VariableDecl.variableDecl list

let to_string (local_var_decl : localVarDecl) (depth : int) =
  match local_var_decl with
  | LocalVarDecl (modifier, apexType, decls) ->
      "(LocalVarDecl\n"
      ^ Formatter.tabs (depth + 1)
      ^ Modifier.to_string modifier (depth + 1)
      ^ ",\n"
      ^ Formatter.tabs (depth + 1)
      ^ ApexType.to_string apexType (depth + 1)
      ^ ",\n"
      ^ Formatter.tabs (depth + 1)
      ^ VariableDecl.to_strings decls (depth + 1)
      ^ ")"

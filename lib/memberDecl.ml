type memberDeclaration =
  | FieldDeclaration of ApexType.node * VariableDecl.variableDecl list
  | MethodDeclaration of ApexType.node * Identifier.node * Stmt.statement list

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
      ^ Stmt.to_strings stmts (depth + 1)
      ^ ")"
  | _ -> "Not implemented yet!"

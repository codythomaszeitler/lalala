type classBodyDeclaration =
  | ClassBodyDeclaration of Modifier.node * MemberDecl.memberDeclaration

let to_string (class_body_decl : classBodyDeclaration) (depth : int) =
  match class_body_decl with
  | ClassBodyDeclaration (modifier, method_decl) ->
      "(ClassBodyDeclaration\n"
      ^ Formatter.tabs (depth + 1)
      ^ Modifier.to_string modifier (depth + 1)
      ^ ",\n" ^ Formatter.tabs depth
      ^ MemberDecl.to_string method_decl (depth + 1)
      ^ ")"

let to_strings (class_body_decls : classBodyDeclaration list) (depth : int) =
  match class_body_decls with
  | [] -> Formatter.tabs depth ^ "[]"
  | _ -> "Not implemented with class body decls to string"

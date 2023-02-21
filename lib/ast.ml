type classBodyDeclaration =
  | ClassBodyDeclaration of Modifier.node * MemberDecl.memberDeclaration

module ClassBodyDeclaration = struct
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

type compilationUnit = TypeDecl of Modifier.node list * classDeclaration

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

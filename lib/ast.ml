type classDeclaration =
  | ClassDeclaration of Identifier.node *  ClassBodyDecl.classBodyDeclaration list

module ClassDeclaration = struct
  let to_string (class_decl : classDeclaration) (depth : int) =
    match class_decl with
    | ClassDeclaration (identifer, class_body_decls) ->
        Formatter.tabs depth ^ "(ClassDeclaration\n"
        ^ Formatter.tabs (depth + 1)
        ^ Identifier.to_string identifer (depth + 1)
        ^ ",\n" ^ Formatter.tabs depth
        ^ ClassBodyDecl.to_strings class_body_decls depth
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

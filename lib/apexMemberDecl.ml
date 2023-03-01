type apexMemberDeclaration =
  | ApexFieldDeclaration of
      Location.location
      * ApexType.apexType
      * ApexVariableDecl.apexVariableDecl list
  | ApexMethodDeclaration of
      Location.location
      * ApexType.apexType
      * ApexIdentifier.apexIdentifier
      * Stmt.stmt list

let pr_member_decl (ppf : Format.formatter)
    (member_decl : apexMemberDeclaration) : unit =
  match member_decl with
  | ApexMethodDeclaration (location, apex_type, identifier, stmts) ->
      Format.fprintf ppf
        "@[<v 2>(ApexMethodDeclaration{@;\
         @[<v 2>apex_type=@;\
         %a@]@;\
         @[<v 2>identifier=@;\
         %a@]@;\
         @[<v 2>stmts=@;\
         [%a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        ApexType.pr_apex_type apex_type ApexIdentifier.pr_identifer identifier
        (Format.pp_print_list Stmt.pr_stmt)
        stmts Location.pr_location location
  | ApexFieldDeclaration (location, apex_type, var_decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexFieldDeclaration{@;\
         @[<v 2>apex_type=@;\
         %a@]@;\
         @[<v 2>decls=[@;\
         %a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        ApexType.pr_apex_type apex_type
        (Format.pp_print_list ApexVariableDecl.pr_variable_decl)
        var_decls Location.pr_location location

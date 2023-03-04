type apexDecl =
  | ApexFieldDeclaration of
      Location.location
      * ApexAnnotation.apexAnnotation option
      * ApexModifier.modifier list
      * ApexType.apexType
      * ApexVariableDecl.apexVariableDecl list
  | ApexMethodDeclaration of
      Location.location
      * ApexAnnotation.apexAnnotation option
      * ApexModifier.modifier list
      * ApexType.apexType
      * ApexIdentifier.apexIdentifier
      * Stmt.stmt list

let pr_member_decl (ppf : Format.formatter) (member_decl : apexDecl) : unit =
  match member_decl with
  | ApexMethodDeclaration
      (location, annotation, modifiers, apex_type, identifier, stmts) ->
      Format.fprintf ppf
        "@[<v 2>(ApexMethodDeclaration{@;\
         @[<v 2>annotation=@;\
         %a@]@;\
         @[<v 2>modifiers=@;\
         %a@]@;\
         @[<v 2>apex_type=@;\
         %a@]@;\
         @[<v 2>identifier=@;\
         %a@]@;\
         @[<v 2>stmts=@;\
         [%a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        (Format.pp_print_option ApexAnnotation.pr_apex_annotation)
        annotation
        (Format.pp_print_list ApexModifier.pr_modifier)
        modifiers ApexType.pr_apex_type apex_type ApexIdentifier.pr_identifer
        identifier
        (Format.pp_print_list Stmt.pr_stmt)
        stmts Location.pr_location location
  | ApexFieldDeclaration (location, annotation, modifiers, apex_type, var_decls)
    ->
      Format.fprintf ppf
        "@[<v 2>(ApexFieldDeclaration{@;\
         @[<v 2>annotation=@;\
         %a@]@;\
         @[<v 2>modifiers=@;\
         %a@]@;\
         @[<v 2>apex_type=@;\
         %a@]@;\
         @[<v 2>decls=[@;\
         %a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        (Format.pp_print_option ApexAnnotation.pr_apex_annotation)
        annotation
        (Format.pp_print_list ApexModifier.pr_modifier)
        modifiers ApexType.pr_apex_type apex_type
        (Format.pp_print_list ApexVariableDecl.pr_variable_decl)
        var_decls Location.pr_location location

let is_test_method (apex_decl : apexDecl) : bool =
  match apex_decl with
  | ApexMethodDeclaration
      (_, Some (ApexAnnotation (_, "TestMethod")), _, _, _, _) ->
      true
  | _ -> false

let has_test_method (apex_decls : apexDecl list) : bool =
  List.exists is_test_method apex_decls

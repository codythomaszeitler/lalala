type classDeclaration =
  | ClassDeclaration of
      Location.location
      * ApexIdentifier.apexIdentifier
      * ClassBodyDecl.apexClassBodyDecl list

let pr_class_decl (ppf : Format.formatter) (class_decl : classDeclaration) :
    unit =
  match class_decl with
  | ClassDeclaration (loc, id, class_body_decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassDeclaration{@;\
         @[id=%a;@]@;\
         @[class_body_decls=[@;%a]@]@;\
         @[location=%a@]})@]"
        ApexIdentifier.pr_identifer id
        (Format.pp_print_list ClassBodyDecl.pr_class_body_decl)
        class_body_decls Location.pr_location loc

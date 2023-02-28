type apexClassDeclaration =
  | ClassDeclaration of
      Location.location
      * ApexIdentifier.apexIdentifier
      * ApexClassBodyDecl.apexClassBodyDecl list

let pr_class_decl (ppf : Format.formatter) (class_decl : apexClassDeclaration) :
    unit =
  match class_decl with
  | ClassDeclaration (loc, id, class_body_decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassDeclaration{@;\
         @[id=%a;@]@;\
         @[class_body_decls=[@;%a]@]@;\
         @[location=%a@]})@]"
        ApexIdentifier.pr_identifer id
        (Format.pp_print_list ApexClassBodyDecl.pr_class_body_decl)
        class_body_decls Location.pr_location loc

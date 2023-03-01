type compilationUnit =
  | ApexClassDeclaration of
      Location.location
      * ApexIdentifier.apexIdentifier
      * ApexClassBodyDecl.apexClassBodyDecl list

let pr_compilation_unit (ppf : Format.formatter)
    (compilation_unit : compilationUnit) : unit =
  match compilation_unit with
  | ApexClassDeclaration (loc, id, class_body_decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassDeclaration{@;\
         @[id=%a;@]@;\
         @[class_body_decls=[@;\
         %a]@]@;\
         @[location=%a@]})@]"
        ApexIdentifier.pr_identifer id
        (Format.pp_print_list ApexClassBodyDecl.pr_class_body_decl)
        class_body_decls Location.pr_location loc

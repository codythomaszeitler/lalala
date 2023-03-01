type compilationUnit =
  | ApexClassDeclaration of
      Location.location
      * ApexAnnotation.apexAnnotation option
      * ApexModifier.modifier list
      * ApexIdentifier.apexIdentifier
      * ApexClassBodyDecl.apexClassBodyDecl list

let pr_compilation_unit (ppf : Format.formatter)
    (compilation_unit : compilationUnit) : unit =
  match compilation_unit with
  | ApexClassDeclaration (loc, annotation, modis, id, class_body_decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassDeclaration{@;\
         @[<v 2>annotation=@;\
         %a@]@;\
         @[<v 2>modis=@;\
         [%a]@]@;\
         @[<v 2>id=@;\
         %a@]@;\
         @[<v 2>class_body_decls\
         =@;[%a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        (Format.pp_print_option ApexAnnotation.pr_apex_annotation)
        annotation
        (Format.pp_print_list ApexModifier.pr_modifier)
        modis ApexIdentifier.pr_identifer id
        (Format.pp_print_list ApexClassBodyDecl.pr_class_body_decl)
        class_body_decls Location.pr_location loc

let to_string (compilation_unit : compilationUnit) : string =
  let buffer = Buffer.create 5 in
  let formatter = Format.formatter_of_buffer buffer in
  pr_compilation_unit formatter compilation_unit;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

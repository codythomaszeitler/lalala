type compilationUnit =
  | ApexClassDeclaration of
      Location.location
      * ApexAnnotation.apexAnnotation option
      * ApexModifier.modifier list
      * ApexIdentifier.apexIdentifier
      * ApexDecl.apexDecl list

let pr_compilation_unit (ppf : Format.formatter)
    (compilation_unit : compilationUnit) : unit =
  match compilation_unit with
  | ApexClassDeclaration (loc, annotation, modis, id, decls) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassDeclaration{@;\
         @[<v 2>annotation=@;\
         %a@]@;\
         @[<v 2>modis=@;\
         [%a]@]@;\
         @[<v 2>id=@;\
         %a@]@;\
         @[<v 2>class_body_decls=@;\
         [%a]@]@;\
         @[<v 2>location=@;\
         %a@]})@]"
        (Format.pp_print_option ApexAnnotation.pr_apex_annotation)
        annotation
        (Format.pp_print_list ApexModifier.pr_modifier)
        modis ApexIdentifier.pr_identifer id
        (Format.pp_print_list ApexDecl.pr_member_decl)
        decls Location.pr_location loc

let to_string (compilation_unit : compilationUnit) : string =
  let buffer = Buffer.create 5 in
  let formatter = Format.formatter_of_buffer buffer in
  pr_compilation_unit formatter compilation_unit;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

let is_test_class (apex : compilationUnit) : bool =
  match apex with
  | ApexClassDeclaration (_, annotation, _, _, _) -> (
      match annotation with
      | Some (ApexAnnotation (_, "IsTest")) -> true
      | _ -> false)

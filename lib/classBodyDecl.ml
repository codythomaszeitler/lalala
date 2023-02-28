type apexClassBodyDecl =
  | ApexClassBodyDeclaration of
      Location.location
      * ApexModifier.modifier
      * ApexMemberDecl.apexMemberDeclaration

let pr_class_body_decl (ppf : Format.formatter)
    (class_body_decl : apexClassBodyDecl) : unit =
  match class_body_decl with
  | ApexClassBodyDeclaration (location, modifier, member_decl) ->
      Format.fprintf ppf
        "@[<v 2>(ApexClassBodyDeclaration{@;\
         @[<v 2>modifier=@;\
         %a;@]@;\
         @[<v 2> member_decl=@;\
         %a;@]@;\
         @[<v 2>location=@;\
         %a@]@]"
        ApexModifier.pr_modifier modifier ApexMemberDecl.pr_member_decl
        member_decl Location.pr_location location

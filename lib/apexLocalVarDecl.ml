type apexLocalVarDecl =
  | ApexLocalVarDecl of
      Location.location
      * ApexModifier.modifier
      * ApexType.apexType
      * ApexVariableDecl.apexVariableDecl list

let pr_local_var_decl (ppf : Format.formatter)
    (apex_local_var_decl : apexLocalVarDecl) : unit =
  match apex_local_var_decl with
  | ApexLocalVarDecl (location, modifier, apex_type, variable_decl) ->
      Format.fprintf ppf
        "@[<hv 2>(ApexLocalVarDecl{@;\
         @[<v 2>modifier=@;%a;@]@;\
         @[<v 2>apex_type=@;%a;@]@;\
         @[<v 2>variable_decl=[@;%a];@]@;\
         @[<v 2>location=@;%a;@]@]})"
        ApexModifier.pr_modifier modifier ApexType.pr_apex_type apex_type
        (Format.pp_print_list ApexVariableDecl.pr_variable_decl)
        variable_decl Location.pr_location location

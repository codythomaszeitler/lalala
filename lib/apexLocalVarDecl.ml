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
        "(ApexLocalVarDecl{@[<v>@;\
         modifier=%a;@;\
         apex_type=%a;@;\
         variable_decl=@;[%a];@;\
         location=%a;@]})"
        ApexModifier.pr_modifier modifier ApexType.pr_apex_type apex_type
        (Format.pp_print_list ApexVariableDecl.pr_variable_decl)
        variable_decl Location.pr_location location

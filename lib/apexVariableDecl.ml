type variableDecl =
  | ApexVariableDecl of Location.location * ApexIdentifier.apexIdentifier

let pr_variable_decl (ppf : Format.formatter) (var_decl : variableDecl) : unit =
  match var_decl with
  | ApexVariableDecl (location, identifier) ->
      Format.fprintf ppf "(ApexVariableDecl@[<v>{@;id=%a;@;loc=%a}@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

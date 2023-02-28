type apexVariableDecl =
  | ApexVariableDecl of Location.location * ApexIdentifier.apexIdentifier

let pr_variable_decl (ppf : Format.formatter) (var_decl : apexVariableDecl) :
    unit =
  match var_decl with
  | ApexVariableDecl (location, identifier) ->
      Format.fprintf ppf
        "@[<v 2>(ApexVariableDecl{@;@[<v 2>id=@;%a;@]@;@[<v 2>loc=@;%a}@]@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

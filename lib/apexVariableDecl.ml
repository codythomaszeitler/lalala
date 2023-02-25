type variableDecl = {
  id : ApexIdentifier.apexIdentifier;
  loc : Location.location option;
}

let create ?(loc = None) (id : ApexIdentifier.apexIdentifier) : variableDecl =
  { id; loc }

let pr_variable_decl (ppf : Format.formatter) (var_decl : variableDecl) : unit =
  match var_decl with
  | { id; loc } ->
      Format.fprintf ppf "(ApexVariableDecl@;<1 2>@[{id=%a;@;<1 2>loc=%a}@])"
        ApexIdentifier.pr_identifer id Location.pr_location loc

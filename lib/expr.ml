type expr = Primary of Location.location * ApexLiteral.apexLiteral

let pr_expr (ppf : Format.formatter) (expr : expr) : unit =
  match expr with
  | Primary (location, literal) ->
      Format.fprintf ppf "(Primary@;<1 2>@[apexLiteral=%a;@;<1 2>loc=%a@])"
        ApexLiteral.pr_apex_literal literal Location.pr_location location

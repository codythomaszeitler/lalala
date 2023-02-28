type expr =
  | Primary of Location.location * ApexLiteral.apexLiteral
  | Binary of Location.location * expr * Operator.operator * expr

let rec pr_expr (ppf : Format.formatter) (expr : expr) : unit =
  match expr with
  | Primary (location, literal) ->
      Format.fprintf ppf "(Primary {@[<v>@;apexLiteral=%a;@;loc=%a@;@]})"
        ApexLiteral.pr_apex_literal literal Location.pr_location location
  | Binary (location, left, op, right) ->
      Format.fprintf ppf
        "(Binary{@[<v>@;@[left=%a;@]@;@[op=%a;@]@;@[right=%a;@]@;@[loc=%a@]})@]"
        pr_expr left Operator.pr_operator op pr_expr right Location.pr_location
        location

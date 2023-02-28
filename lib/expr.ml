type expr =
  | Primary of Location.location * ApexLiteral.apexLiteral
  | Binary of Location.location * expr * Operator.operator * expr

let rec pr_expr (ppf : Format.formatter) (expr : expr) : unit =
  match expr with
  | Primary (location, literal) ->
      Format.fprintf ppf "@[<v 2>(Primary{@;@[<v 2>apexLiteral=@;%a;@]@;loc=%a@]})"
        ApexLiteral.pr_apex_literal literal Location.pr_location location
  | Binary (location, left, op, right) ->
      Format.fprintf ppf
        "@[<v 2>(Binary{@;@[<v 2>left=@;%a;@]@;@[<v 2>op=@;%a;@]@;@[<v 2>right=@;%a;@]@;@[<v 2>loc=@;%a@]})@]"
        pr_expr left Operator.pr_operator op pr_expr right Location.pr_location
        location

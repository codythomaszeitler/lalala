type expr =
  | Id of Location.location * ApexIdentifier.apexIdentifier
  | Literal of Location.location * ApexLiteral.apexLiteral
  | Binary of Location.location * expr * Operator.operator * expr

let rec pr_expr (ppf : Format.formatter) (expr : expr) : unit =
  match expr with
  | Id (location, identifier) ->
      Format.fprintf ppf "@[<v 2>(Id{@;@[<v 2>identifier=@;%a;@]@;loc=%a@]})"
        ApexIdentifier.pr_identifer identifier Location.pr_location location
  | Literal (location, literal) ->
      Format.fprintf ppf "@[<v 2>(Literal{@;@[<v 2>literal=@;%a;@]@;loc=%a@]})"
        ApexLiteral.pr_apex_literal literal Location.pr_location location
  | Binary (location, left, op, right) ->
      Format.fprintf ppf
        "@[<v 2>(Binary{@;\
         @[<v 2>left=@;\
         %a;@]@;\
         @[<v 2>op=@;\
         %a;@]@;\
         @[<v 2>right=@;\
         %a;@]@;\
         @[<v 2>loc=@;\
         %a@]})@]"
        pr_expr left Operator.pr_operator op pr_expr right Location.pr_location
        location

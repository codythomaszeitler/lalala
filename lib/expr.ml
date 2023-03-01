open Location

type expr =
  | Id of Location.location * ApexIdentifier.apexIdentifier
  | Binary of Location.location * expr * Operator.operator * expr
  | IntegerLiteral of Location.location * int
  | LongLiteral of Location.location * int
  | StringLiteral of Location.location * string
  | BooleanLiteral of Location.location * bool
  | NullLiteral of Location.location

let rec pr_expr (ppf : Format.formatter) (expr : expr) : unit =
  match expr with
  | Id (location, identifier) ->
      Format.fprintf ppf "@[<v 2>(Id{@;@[<v 2>identifier=@;%a;@]@;loc=%a@]})"
        ApexIdentifier.pr_identifer identifier Location.pr_location location
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
  | IntegerLiteral (location, int) ->
      Format.fprintf ppf "@[<v 2>(IntegerLiteral{@;int=%d;@;loc=%a@]})" int
        pr_location location
  | LongLiteral (location, long) ->
      Format.fprintf ppf "@[<v 2>(LongLiteral{@;long=%d;@;loc=%a@]})" long
        pr_location location
  | StringLiteral (location, string) ->
      Format.fprintf ppf "@[<v 2>(StringLiteral{@;string=\"%s\";@;loc=%a@]})"
        string pr_location location
  | BooleanLiteral (location, bool) ->
      Format.fprintf ppf "@[<v 2>(BooleanLiteral{@;bool=%b;@;loc=%a@]})" bool
        pr_location location
  | NullLiteral location ->
      Format.fprintf ppf "@[<v 2>(NullLiteral{@;loc=%a@]})" pr_location location

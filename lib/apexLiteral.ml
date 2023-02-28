open Location

type apexLiteral =
  | IntegerLiteral of Location.location * int
  | LongLiteral of Location.location * int
  | StringLiteral of Location.location * string
  | BooleanLiteral of Location.location * bool
  | NullLiteral of Location.location

let pr_apex_literal (ppf : Format.formatter) (apex_literal : apexLiteral) : unit
    =
  match apex_literal with
  | IntegerLiteral (location, int) ->
      Format.fprintf ppf "(IntegerLiteral{@[<v>@;int=%d;@;loc=%a@;@]})" int
        pr_location location
  | LongLiteral (location, long) ->
      Format.fprintf ppf "(LongLiteral{@[<v>@;long=%d;@;loc=%a@;@]})" long
        pr_location location
  | StringLiteral (location, string) ->
      Format.fprintf ppf "(StringLiteral{@[<v>@;string=\"%s\";@;loc=%a@;@]})"
        string pr_location location
  | BooleanLiteral (location, bool) ->
      Format.fprintf ppf "(BooleanLiteral{@[<v>@;bool=%b;@;loc=%a@;@]})" bool
        pr_location location
  | NullLiteral location ->
      Format.fprintf ppf "(NullLiteral{@[<v>@;loc=%a@;@]})" pr_location location

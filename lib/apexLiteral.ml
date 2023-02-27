open Location

type apexLiteral =
  | IntegerLiteral of Location.location * int
  | LongLiteral of Location.location * int
  | StringLiteral of Location.location * string

let pr_apex_literal (ppf : Format.formatter) (apex_literal : apexLiteral) : unit
    =
  match apex_literal with
  | IntegerLiteral (location, int) ->
      Format.fprintf ppf "(IntegerLiteral@;<1 2>@[{int=%d;@;<1 2>loc=%a}@])" int
        pr_location location
  | LongLiteral (location, long) ->
      Format.fprintf ppf "(LongLiteral@;<1 2>@[{long=%d;@;<1 2>loc=%a}@])" long
        pr_location location
  | StringLiteral (location, string) ->
      Format.fprintf ppf
        "(StringLiteral@;<1 2>@[{string=\"%s\";@;<1 2>loc=%a}@])" string
        pr_location location

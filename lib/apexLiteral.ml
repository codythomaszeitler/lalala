open Location

type apexLiteral = IntegerLiteral of Location.location * int

let pr_apex_literal (ppf : Format.formatter) (apex_literal : apexLiteral) : unit
    =
  match apex_literal with
  | IntegerLiteral (location, int) ->
      Format.fprintf ppf "(IntegerLiteral@;<1 2>@[{int=%d;@;<1 2>loc=%a}@])" int
        pr_location location
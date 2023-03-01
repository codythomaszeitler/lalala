type apexPrimary =
  | Literal of Location.location * ApexLiteral.apexLiteral
  | Id of Location.location * ApexIdentifier.apexIdentifier

let pr_primary (ppf : Format.formatter) (primary : apexPrimary) : unit =
  match primary with
  | Literal (location, literal) ->
      Format.fprintf ppf
        "@[<v 2>(Literal{@;@[<v 2>literal=%a@]@;@[<v 2>location=%a@]})@]"
        ApexLiteral.pr_apex_literal literal Location.pr_location location
  | Id (location, identifier) ->
      Format.fprintf ppf "@[<v 2>(Id{@;@[<v 2>id=%a@]@;@[<v 2>location=%a@]})@]"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

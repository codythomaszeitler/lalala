type apexIdentifier = ApexIdentifier of Location.location * string

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | ApexIdentifier (location, name) ->
      Format.fprintf ppf "(ApexIdentifier@;<1 2>@[{name=\"%s\"@;<1 2>loc=%a@]})"
        name Location.pr_location location

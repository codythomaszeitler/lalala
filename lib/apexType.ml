type apexType = ApexType of Location.location * ApexIdentifier.apexIdentifier

let pr_apex_type (ppf : Format.formatter) (apex_type : apexType) : unit =
  match apex_type with
  | ApexType (location, identifier) ->
      Format.fprintf ppf "(ApexType@;<1 2>@[{identifier=%a;@;<1 2>loc=%a}@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

type apexType = ApexType of Location.location * ApexIdentifier.apexIdentifier

let pr_apex_type (ppf : Format.formatter) (apex_type : apexType) : unit =
  match apex_type with
  | ApexType (location, identifier) ->
      Format.fprintf ppf "@[<v 2>(ApexType{@;@[<v 2>identifier=@;%a;@]@;loc=%a}@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

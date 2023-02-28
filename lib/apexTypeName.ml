type apexTypeName = ApexTypeName of Location.location * ApexIdentifier.apexIdentifier

let pr_apex_type (ppf : Format.formatter) (apex_type_name : apexTypeName) : unit =
  match apex_type_name with
  | ApexTypeName (location, identifier) ->
      Format.fprintf ppf "@[<v 2>(ApexTypeName{@;@[<v 2>identifier=@;%a;@]@;loc=%a}@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location location

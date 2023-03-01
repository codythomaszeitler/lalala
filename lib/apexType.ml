type apexType = ApexType of Location.location * string

let pr_apex_type (ppf : Format.formatter) (apex_type_name : apexType) : unit
    =
  match apex_type_name with
  | ApexType (location, name) ->
      Format.fprintf ppf
        "@[<v 2>(ApexType{@;@[<v 2>identifier=@;%s;@]@;loc=%a}@])" name
        Location.pr_location location

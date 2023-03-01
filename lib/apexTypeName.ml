type apexTypeName = ApexTypeName of Location.location * string

let pr_apex_type (ppf : Format.formatter) (apex_type_name : apexTypeName) : unit
    =
  match apex_type_name with
  | ApexTypeName (location, name) ->
      Format.fprintf ppf
        "@[<v 2>(ApexTypeName{@;@[<v 2>identifier=@;%s;@]@;loc=%a}@])" name
        Location.pr_location location

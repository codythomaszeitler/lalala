type apexIdentifier = ApexIdentifier of Location.location * string

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | ApexIdentifier (location, name) ->
      Format.fprintf ppf "@[<v 2>(ApexIdentifier{@;@[<v 2>name=@;\"%s\"@]@;@[<v 2>loc=@;%a@]@]})"
        name Location.pr_location location

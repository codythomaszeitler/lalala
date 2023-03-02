type apexIdentifier = ApexIdentifier of Location.location * string

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | ApexIdentifier (location, name) ->
      Format.fprintf ppf
        "@[<v 2>(ApexIdentifier{@;@[<v 2>name=@;\"%s\"@]@;@[<v 2>loc=@;%a@]@]})"
        name Location.pr_location location

let build_qualified_name (identifiers : apexIdentifier list) : apexIdentifier =
  let get_name = function ApexIdentifier (_, name) -> name in
  let rec get_qualified_name_string (identifiers : apexIdentifier list) : string
      =
    match identifiers with
    | h :: [] -> get_name h
    | h :: t -> get_name h ^ "." ^ get_qualified_name_string t
    | [] -> ""
  in
  ApexIdentifier (Location.no_loc, get_qualified_name_string identifiers)

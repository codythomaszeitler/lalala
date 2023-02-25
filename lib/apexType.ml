type apexType = {
  identifier : ApexIdentifier.apexIdentifier;
  loc : Location.location option;
}

let create ?(loc = None) (identifier : ApexIdentifier.apexIdentifier) : apexType
    =
  { identifier; loc }

let pr_apex_type (ppf : Format.formatter) (apex_type : apexType) : unit =
  match apex_type with
  | { identifier; loc } ->
      Format.fprintf ppf "(ApexType@;<1 2>@[{identifier=%a;@;<1 2>loc=%a}@])"
        ApexIdentifier.pr_identifer identifier Location.pr_location loc

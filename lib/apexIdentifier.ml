type apexIdentifier = { name : string; loc : Location.location option}

let create ?(loc = None) (name : string) :
    apexIdentifier =
  { name; loc }

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | { name; loc } -> Format.fprintf ppf "(ApexIdentifier {@[ name=\"%s\" loc=%a@]})" name Location.pr_location loc

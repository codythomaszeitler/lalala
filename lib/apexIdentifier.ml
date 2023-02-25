type apexIdentifier = { name : string; loc : Location.location option}

let create ?(loc = None) (name : string) :
    apexIdentifier =
  { name; loc }

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | { name; loc } -> Format.fprintf ppf "(ApexIdentifier@;<1 2>@[{name=\"%s\"@;<1 2>loc=%a@]})" name Location.pr_location loc

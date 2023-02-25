type apexIdentifier = { name : string }

let create (name : string) : apexIdentifier = { name }

let pr_identifer (ppf : Format.formatter) (id : apexIdentifier) =
  match id with
  | { name } -> Format.fprintf ppf "(ApexIdentifier {@[ name=\"%s\" @]})" name

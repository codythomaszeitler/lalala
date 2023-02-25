type identifier = { name : string }

let create (name : string) : identifier = { name }

let pr_identifer (ppf : Format.formatter) (id : identifier) =
  match id with
  | { name } -> Format.fprintf ppf "(Identifier {@[ name=\"%s\" @]})" name

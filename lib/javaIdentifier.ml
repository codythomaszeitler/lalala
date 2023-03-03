type javaIdentifier = JavaIdentifier of string

let pr_java_identifier (ppf : Format.formatter) (java_type : javaIdentifier) :
    unit =
  match java_type with
  | JavaIdentifier name ->
      Format.fprintf ppf "@[<v 2>(JavaIdentifier{@;name=\"%s\"})@]" name

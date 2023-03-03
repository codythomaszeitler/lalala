type javaType = JavaType of string

let pr_java_type (ppf : Format.formatter) (java_type : javaType) : unit =
  match java_type with
  | JavaType name ->
      Format.fprintf ppf "@[<v 2>(JavaType{@;name=\"%s\"})@]" name

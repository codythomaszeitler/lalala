type javaAnnotation = JavaAnnotation of string
let pr_java_annotation (ppf : Format.formatter) (java_type : javaAnnotation) :
    unit =
  match java_type with
  | JavaAnnotation name ->
      Format.fprintf ppf "@[<v 2>(JavaAnnotation{@;name=\"%s\"})@]" name
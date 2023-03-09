type javaAnnotation = JavaTest

let pr_java_annotation (ppf : Format.formatter) (java_type : javaAnnotation) :
    unit =
  match java_type with JavaTest -> Format.fprintf ppf "@[<v 2>(JavaTest)@]"

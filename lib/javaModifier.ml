type javaModifier = JavaPublic | JavaPrivate | JavaStatic

let pr_java_modifier (ppf : Format.formatter) (java_type : javaModifier) : unit
    =
  match java_type with
  | JavaPublic -> Format.fprintf ppf "@[<v 2>(JavaPublic)@]"
  | JavaPrivate -> Format.fprintf ppf "@[<v 2>(JavaPrivate)@]"
  | JavaStatic -> Format.fprintf ppf "@[<v 2>(JavaStatic)@]"

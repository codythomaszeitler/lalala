type javaImport = JavaImport of string

let pr_java_import (ppf : Format.formatter) (java_import : javaImport) : unit =
  match java_import with
  | JavaImport name ->
      Format.fprintf ppf "@[<v 2>(JavaImport{@;module=%s})@]" name

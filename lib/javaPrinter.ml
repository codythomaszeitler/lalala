open Java

let print_import (ppf : Format.formatter) (import : javaImport) : unit =
  match import with JavaImport name -> Format.fprintf ppf "import %s;" name

let print_identifier (ppf : Format.formatter) (identifier : javaIdentifier) :
    unit =
  match identifier with JavaIdentifier name -> Format.fprintf ppf "%s" name

let print_decl (ppf : Format.formatter) (decl : javaDecl) : unit =
  match decl with
  | JavaClassDecl (_, _, identifier, _) ->
      Format.fprintf ppf "class %a {}" print_identifier identifier
  | _ -> Format.fprintf ppf ""

let print_newline ppf () = Format.fprintf ppf "@;"

let print (ppf : Format.formatter) (java : java) : unit =
  match java with
  | JavaFile (imports, decl) ->
      Format.fprintf ppf "@[<v>%a@;@;%a@;@]"
        (Format.pp_print_list ~pp_sep:print_newline print_import)
        imports
        print_decl decl

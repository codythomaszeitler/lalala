include JavaAnnotation
include JavaType
include JavaIdentifier
include JavaModifier
include JavaExpr
include JavaStmt
include JavaDecl
include JavaImport

type java = JavaFile of javaImport list * javaDecl

let pr_java (ppf : Format.formatter) (java : java) : unit =
  match java with
  | JavaFile (imports, decl) ->
      Format.fprintf ppf "@[<v 2>(Java{@;imports=%a@;class_decl=%a})@]"
        (Format.pp_print_list pr_java_import)
        imports pr_java_decl decl

let to_string_java (java : java) : string =
  let buffer = Buffer.create 5 in
  let formatter = Format.formatter_of_buffer buffer in
  pr_java formatter java;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

include JavaAnnotation
include JavaType
include JavaIdentifier
include JavaModifier
include JavaExpr
include JavaStmt
include JavaDecl

type java = JavaFile of javaDecl

let pr_java (ppf : Format.formatter) (java : java) : unit =
  match java with
  | JavaFile decl ->
      Format.fprintf ppf "@[<v 2>(Java{@;class_decl=%a})@]" pr_java_decl decl

let to_string_java (java : java) : string =
  let buffer = Buffer.create 5 in
  let formatter = Format.formatter_of_buffer buffer in
  pr_java formatter java;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

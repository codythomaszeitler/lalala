open Java

let print_import (ppf : Format.formatter) (import : javaImport) : unit =
  match import with JavaImport name -> Format.fprintf ppf "import %s;" name

let print_identifier (ppf : Format.formatter) (identifier : javaIdentifier) :
    unit =
  match identifier with JavaIdentifier name -> Format.fprintf ppf "%s" name

let print_annotation (ppf : Format.formatter) (java_annotation : javaAnnotation)
    : unit =
  match java_annotation with JavaTest -> Format.fprintf ppf "%@%s" "Test"

let print_modifier (ppf : Format.formatter) (java_modifier : javaModifier) :
    unit =
  match java_modifier with
  | JavaPrivate -> Format.fprintf ppf "%s" "private"
  | JavaPublic -> Format.fprintf ppf "%s" "public"
  | JavaStatic -> Format.fprintf ppf "%s" "static"

let print_type (ppf : Format.formatter) (java_type : javaType) : unit =
  match java_type with JavaType name -> Format.fprintf ppf "%s" name

let print_with_comma ppf () = Format.fprintf ppf ","

let rec print_expr (ppf : Format.formatter) (expr : javaExpr) : unit =
  match expr with
  | JavaMethodCall (identifier, exprs) ->
      Format.fprintf ppf "%a(%a)" print_identifier identifier
        (Format.pp_print_list ~pp_sep:print_with_comma print_expr)
        exprs
  | JavaIntegerLiteral number -> Format.fprintf ppf "%d" number

let print_statement (ppf : Format.formatter) (java_stmt : javaStmt) : unit =
  match java_stmt with
  | JavaExprStmt expr -> Format.fprintf ppf "%a;" print_expr expr
  | _ -> Format.fprintf ppf ""

let rec print_decl (ppf : Format.formatter) (decl : javaDecl) : unit =
  match decl with
  | JavaClassDecl (_, _, identifier, decls) ->
      Format.fprintf ppf "@[<v 2>class %a {@;%a@]@.}" print_identifier
        identifier
        (Format.pp_print_list print_decl)
        decls
  | JavaMethodDecl (annotation, modifier, java_type, identifier, stmts) ->
      Format.fprintf ppf "@[%a@ %a@ %a@ %a()@ {@;%a}@]"
        (Format.pp_print_option print_annotation)
        annotation
        (Format.pp_print_option print_modifier)
        modifier print_type java_type print_identifier identifier
        (Format.pp_print_list print_statement)
        stmts
  | _ -> Format.fprintf ppf ""

let print_newline ppf () = Format.fprintf ppf "@;"

let print (ppf : Format.formatter) (java : java) : unit =
  match java with
  | JavaFile (imports, decl) ->
      Format.fprintf ppf "@[<v>%a@;@;%a@;@]"
        (Format.pp_print_list ~pp_sep:print_newline print_import)
        imports print_decl decl

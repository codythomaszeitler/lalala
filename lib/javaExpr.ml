open JavaIdentifier

type javaExpr =
  | JavaMethodCall of javaIdentifier * javaExpr list
  | JavaIntegerLiteral of int

let rec pr_java_expr (ppf : Format.formatter) (java_expr : javaExpr) : unit =
  match java_expr with
  | JavaMethodCall (identifier, exprs) ->
      Format.fprintf ppf "@[<v 2>(JavaMethodCall{@;identifier=%a@;exprs=%a@]"
        pr_java_identifier identifier
        (Format.pp_print_list pr_java_expr)
        exprs
  | JavaIntegerLiteral number ->
      Format.fprintf ppf "@[<v 2>(JavaIntegerLiteral{@;number=%d}@]" number

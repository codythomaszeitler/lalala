open JavaExpr

type javaStmt = JavaReturnStmt | JavaExprStmt of javaExpr

let pr_java_stmt (ppf : Format.formatter) (java_stmt : javaStmt) : unit =
  match java_stmt with
  | JavaReturnStmt -> Format.fprintf ppf "@[<v 2>(JavaReturnStmt)@]"
  | JavaExprStmt expr ->
      Format.fprintf ppf "@[<v 2>(JavaExprStmt{expr=%a})@]" pr_java_expr expr

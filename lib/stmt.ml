type stmt =
  (* | LocalVarDeclStmt of ApexLocalVarDecl.apexLocalVarDecl *)
  | ReturnStmt of Location.location * Expr.expr

let pr_stmt (ppf : Format.formatter) (stmt : stmt) : unit =
  match stmt with
  | ReturnStmt (location, expr) ->
      Format.fprintf ppf "(ReturnStmt{@[<v>@;expr=%a;@;location=%a;@]})"
        Expr.pr_expr expr Location.pr_location location

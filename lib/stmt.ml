type stmt =
  | LocalVarDeclStmt of Location.location * ApexLocalVarDecl.apexLocalVarDecl
  | ReturnStmt of Location.location * Expr.expr

let pr_stmt (ppf : Format.formatter) (stmt : stmt) : unit =
  match stmt with
  | ReturnStmt (location, expr) ->
      Format.fprintf ppf "@[<v 2>(ReturnStmt{@;@[<v 2>expr=@;%a@];@;@[<v 2>location=%a;@]@]})"
        Expr.pr_expr expr Location.pr_location location
  | LocalVarDeclStmt (location, local_var_decl) ->
      Format.fprintf ppf
        "@[<hv 2>(LocalVarDeclStmt{@;@[<hv 2>local_var_decl=@,%a;@]@;@[<hv 2>location=@,%a;@]@]})"
        ApexLocalVarDecl.pr_local_var_decl local_var_decl Location.pr_location
        location

type stmt =
  | ApexLocalVarDeclStmt of
      Location.location * ApexLocalVarDecl.apexLocalVarDecl
  | ApexReturnStmt of Location.location * Expr.expr
  | ApexExprStmt of Location.location * Expr.expr

let pr_stmt (ppf : Format.formatter) (stmt : stmt) : unit =
  match stmt with
  | ApexReturnStmt (location, expr) ->
      Format.fprintf ppf
        "@[<v 2>(ApexReturnStmt{@;\
         @[<v 2>expr=@;\
         %a@];@;\
         @[<v 2>location=%a;@]@]})" Expr.pr_expr expr Location.pr_location
        location
  | ApexLocalVarDeclStmt (location, local_var_decl) ->
      Format.fprintf ppf
        "@[<hv 2>(ApexLocalVarDeclStmt{@;\
         @[<hv 2>local_var_decl=@,\
         %a;@]@;\
         @[<hv 2>location=@,\
         %a;@]@]})"
        ApexLocalVarDecl.pr_local_var_decl local_var_decl Location.pr_location
        location
  | ApexExprStmt (location, expr) ->
      Format.fprintf ppf
        "@[<hv 2>(ApexExprStmt{@;\
         @[<hv 2>expr=@,\
         %a;@]@;\
         @[<hv 2>location=@,\
         %a;@]@]})"
        Expr.pr_expr expr Location.pr_location location

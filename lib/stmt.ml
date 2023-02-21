type statement =
  | LocalVarDeclStmt of LocalVarDecl.localVarDecl
  | ReturnStmt of Expr.node

let to_string (stmt : statement) (depth : int) =
  match stmt with
  | ReturnStmt expr ->
      "(ReturnStmt\n"
      ^ Formatter.tabs (depth + 1)
      ^ Expr.to_string expr (depth + 1)
      ^ ")"
  | _ -> "Not supported yet"

let rec _to_strings (stmts : statement list) (depth : int) =
  match stmts with
  | [] -> ""
  | h :: [] -> Formatter.tabs depth ^ to_string h depth
  | h :: t ->
      Formatter.tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

let to_strings (stmts : statement list) (depth : int) =
  "[\n" ^ _to_strings stmts (depth + 1) ^ "]"

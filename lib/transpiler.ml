open Java
open Ast
open ApexDecl
open ApexAnnotation
open ApexIdentifier
open ApexType
open Expr

let transpile_method_annotation (apex_annotation : apexAnnotation option) :
    javaAnnotation option =
  match apex_annotation with
  | Some (IsTest _) -> Some (JavaAnnotation "Test")
  | _ -> None

let transpile_identifier (apex_identifer : apexIdentifier) : javaIdentifier =
  match apex_identifer with ApexIdentifier (_, name) -> JavaIdentifier name

let transpile_type (apex_type : apexType) : javaType =
  match apex_type with ApexType (_, name) -> JavaType name

let rec transpile_exprs (exprs : expr list) : javaExpr list =
  match exprs with h :: t -> transpile_expr h :: transpile_exprs t | [] -> []

and transpile_expr (expr : expr) : javaExpr =
  match expr with
  | IntegerLiteral (_, number) -> JavaIntegerLiteral number
  | ApexMethodCall (_, identifier, exprs) ->
      JavaMethodCall (transpile_identifier identifier, transpile_exprs exprs)
  | _ -> failwith "transpile expr not implemented yet"

let transpile_stmt (stmt : Stmt.stmt) : javaStmt =
  match stmt with
  | ApexLocalVarDeclStmt (_, _) -> failwith "a"
  | ApexReturnStmt (_, _) -> failwith "b"
  | ApexExprStmt (_, expr) -> JavaExprStmt (transpile_expr expr)

let rec transpile_stmts (stmts : Stmt.stmt list) : javaStmt list =
  match stmts with h :: t -> transpile_stmt h :: transpile_stmts t | [] -> []

let rec transpile_decls (top_level_class : compilationUnit)
    (decls : apexDecl list) : javaDecl list =
  let transpile_decl (apex_decl : apexDecl) : javaDecl =
    match apex_decl with
    | ApexMethodDeclaration (_, annotation, _, apex_type, identifier, stmts) ->
        JavaMethodDecl
          ( transpile_method_annotation annotation,
            (* To do fix this thing *)
            TranspilerModifier.transpile_method_access_modifier apex_decl,
            transpile_type apex_type,
            transpile_identifier identifier,
            transpile_stmts stmts )
    | _ -> failwith "transpile_decl not supported yet"
  in
  match decls with
  | h :: t -> transpile_decl h :: transpile_decls top_level_class t
  | [] -> []

let transpile (apex : compilationUnit) : java =
  match apex with
  | ApexClassDeclaration (_, _, _, identifier, decls) ->
      JavaFile
        ( TranspilerImport.transpile apex,
          JavaClassDecl
            ( None,
              TranspilerModifier.transpile_class_access_modifier apex,
              transpile_identifier identifier,
              transpile_decls apex decls ) )

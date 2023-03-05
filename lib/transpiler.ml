open Java
open Ast
open ApexModifier
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

let rec transpile_modifiers (top_level_class : compilationUnit)
    (modifiers : modifier list) : javaModifier list =
  let transpile_modifier (modifier : modifier) : javaModifier option =
    match top_level_class with
    | ApexClassDeclaration (_, Some (IsTest _), _, _, _) -> (
        match modifier with
        | Private _ -> Some JavaPublic
        | Public _ -> Some JavaPublic
        | _ -> None)
    | _ -> failwith "a"
  in
  match modifiers with
  | h :: t ->
      let modi = transpile_modifier h in
      if Option.is_some modi then
        Option.get modi :: transpile_modifiers top_level_class t
      else transpile_modifiers top_level_class t
  | [] -> []

let get_access_modifier (top_level_class : compilationUnit)
    (modifiers : modifier list) : javaModifier option =
  let modifiers = transpile_modifiers top_level_class modifiers in
  match modifiers with
  | h :: [] -> Some h
  | _ :: _ ->
      failwith "get_access_modifier: There were too many modifiers found"
  | [] -> None

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
    | ApexMethodDeclaration
        (_, annotation, modifiers, apex_type, identifier, stmts) ->
        JavaMethodDecl
          ( transpile_method_annotation annotation,
            get_access_modifier top_level_class modifiers,
            transpile_type apex_type,
            transpile_identifier identifier,
            transpile_stmts stmts )
    | _ -> failwith "transpile_decl not supported yet"
  in
  match decls with
  | h :: t -> transpile_decl h :: transpile_decls top_level_class t
  | [] -> []

let transpile (apex : compilationUnit) : java =
  let transpile_mods = get_access_modifier apex in
  match apex with
  | ApexClassDeclaration (_, _, modifiers, identifier, decls) ->
      JavaFile
        ( TranspilerImport.transpile apex,
          JavaClassDecl
            ( None,
              transpile_mods modifiers,
              transpile_identifier identifier,
              transpile_decls apex decls ) )

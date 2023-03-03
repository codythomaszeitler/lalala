open JavaAnnotation
open JavaType
open JavaIdentifier
open JavaStmt
open JavaModifier

type javaDecl =
  | JavaMethodDecl of
      javaAnnotation option
      * javaModifier option
      * javaType
      * javaIdentifier
      * javaStmt list
  | JavaClassDecl of
      javaAnnotation option
      * javaModifier option
      * javaIdentifier
      * javaDecl list

let rec pr_java_decl (ppf : Format.formatter) (java_decl : javaDecl) : unit =
  match java_decl with
  | JavaMethodDecl (annotation, modifier, java_type, identifier, stmts) ->
      Format.fprintf ppf
        "@[<v 2>(JavaMethodDecl{@;\
         annotation=%a@;\
         modifier=%a;java_type=%a;identifier=%a@;\
         stmts=%a}@]"
        (Format.pp_print_option pr_java_annotation)
        annotation
        (Format.pp_print_option pr_java_modifier)
        modifier pr_java_type java_type pr_java_identifier identifier
        (Format.pp_print_list pr_java_stmt)
        stmts
  | JavaClassDecl (annotation, modifier, identifier, decls) ->
      Format.fprintf ppf
        "@[<v 2>(JavaClassDecl{@;\
         annotation=%a@;\
         modifier=%a;identifier=%a@;\
         decls=%a}@]"
        (Format.pp_print_option pr_java_annotation)
        annotation
        (Format.pp_print_option pr_java_modifier)
        modifier pr_java_identifier identifier
        (Format.pp_print_list pr_java_decl)
        decls

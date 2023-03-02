open Java
open Ast
open ApexModifier
open ApexDecl
open ApexAnnotation
open ApexIdentifier
open ApexType

let transpile_annotation (apex_annotation : apexAnnotation option) :
    javaAnnotation option =
  match apex_annotation with
  | Some (ApexAnnotation (_, "IsTest")) -> None
  | Some (ApexAnnotation (_, "TestMethod")) -> Some (JavaAnnotation "Test")
  | Some (ApexAnnotation (_, name)) -> Some (JavaAnnotation name)
  | _ -> None

let rec transpile_modifiers (top_level_class : compilationUnit)
    (modifiers : modifier list) : javaModifier list =
  let transpile_modifier (modifier : modifier) : javaModifier option =
    match top_level_class with
    | ApexClassDeclaration (_, Some (ApexAnnotation (_, "IsTest")), _, _, _)
      -> (
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

let rec transpile_decls (top_level_class : compilationUnit)
    (decls : apexDecl list) : javaDecl list =
  let transpile_decl (apex_decl : apexDecl) : javaDecl =
    match apex_decl with
    | ApexMethodDeclaration (_, annotation, modifiers, apex_type, identifier, _)
      ->
        JavaMethodDecl
          ( transpile_annotation annotation,
            get_access_modifier top_level_class modifiers,
            transpile_type apex_type,
            transpile_identifier identifier,
            [] )
    | _ -> failwith "transpile_decl not supported yet"
  in
  match decls with
  | h :: t -> transpile_decl h :: transpile_decls top_level_class t
  | [] -> []

let transpile (apex : compilationUnit) : java =
  let transpile_mods = get_access_modifier apex in
  match apex with
  | ApexClassDeclaration (_, annotation, modifiers, identifier, decls) ->
      JavaFile
        (JavaClassDecl
           ( transpile_annotation annotation,
             transpile_mods modifiers,
             transpile_identifier identifier,
             transpile_decls apex decls ))

let pr_java_type (ppf : Format.formatter) (java_type : javaType) : unit =
  match java_type with
  | JavaType name ->
      Format.fprintf ppf "@[<v 2>(JavaType{@;name=\"%s\"})@]" name

let pr_java_annotation (ppf : Format.formatter) (java_type : javaAnnotation) :
    unit =
  match java_type with
  | JavaAnnotation name ->
      Format.fprintf ppf "@[<v 2>(JavaAnnotation{@;name=\"%s\"})@]" name

let pr_java_identifier (ppf : Format.formatter) (java_type : javaIdentifier) :
    unit =
  match java_type with
  | JavaIdentifier name ->
      Format.fprintf ppf "@[<v 2>(JavaIdentifier{@;name=\"%s\"})@]" name

let pr_java_modifier (ppf : Format.formatter) (java_type : javaModifier) : unit
    =
  match java_type with
  | JavaPublic -> Format.fprintf ppf "@[<v 2>(JavaPublic)@]"
  | JavaPrivate -> Format.fprintf ppf "@[<v 2>(JavaPrivate)@]"

let rec pr_java_decl (ppf : Format.formatter) (java_decl : javaDecl) : unit =
  match java_decl with
  | JavaMethodDecl (annotation, modifier, java_type, identifier, _) ->
      Format.fprintf ppf
        "@[<v 2>(JavaMethodDecl{@;\
         annotation=%a@;\
         modifier=%a;java_type=%a;identifier=%a}@]"
        (Format.pp_print_option pr_java_annotation)
        annotation
        (Format.pp_print_option pr_java_modifier)
        modifier pr_java_type java_type pr_java_identifier identifier
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

let pr_java (ppf : Format.formatter) (java : java) : unit =
  match java with
  | JavaFile decl ->
      Format.fprintf ppf "@[<v 2>(Java{@;class_decl=%a})@]" pr_java_decl decl

let to_string_java (java : java) : string =
  let buffer = Buffer.create 5 in
  let formatter = Format.formatter_of_buffer buffer in
  pr_java formatter java;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

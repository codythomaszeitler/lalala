open Java
open Ast

exception ModifierTranspilerException of string * modifier list

let transpile_modifier is_test_check context modifier =
  match modifier with
  | Private _ when is_test_check context -> Some JavaPublic
  | Private _ -> Some JavaPrivate
  | Public _ -> Some JavaPublic
  | Static _ -> Some JavaStatic
  | _ -> None

let get_access_modifier is_test_check context modifiers =
  let access_modifiers =
    List.filter ApexModifier.is_access_modifier modifiers
  in
  let transpiled =
    List.map (transpile_modifier is_test_check context) access_modifiers
  in
  match transpiled with
  | [] -> None
  | [ h ] -> h
  | _ ->
      raise
        (ModifierTranspilerException
           ("Too many access modifiers found", access_modifiers))

let get_class_access_modifier top_level_apex_class =
  let (ApexClassDeclaration (_, _, modifiers, _, _)) = top_level_apex_class in
  get_access_modifier is_test_class top_level_apex_class modifiers

let get_method_access_modifier apex_method_decl =
  let modifiers =
    match apex_method_decl with
    | ApexMethodDeclaration (_, _, modifiers, _, _, _) -> modifiers
    | ApexFieldDeclaration (_, _, modifiers, _, _) -> modifiers
  in
  get_access_modifier is_test_method apex_method_decl modifiers

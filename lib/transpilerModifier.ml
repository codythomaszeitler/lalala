open Java
open ApexModifier
open Ast

exception ModifierTranspilerException of string * modifier list

let transpile_class_modifier top_level_apex_class apex_modifier =
  match top_level_apex_class with
  | ApexClassDeclaration (_, Some (IsTest _), _, _, _) -> (
      match apex_modifier with
      | Private _ -> Some JavaPublic
      | Public _ -> Some JavaPublic
      | Static _ -> Some JavaStatic
      | _ -> None)
  | ApexClassDeclaration (_, _, _, _, _) -> (
      match apex_modifier with
      | Private _ -> Some JavaPrivate
      | Public _ -> Some JavaPublic
      | Static _ -> Some JavaStatic
      | _ -> None)

let get_class_access_modifier top_level_apex_class =
  let (ApexClassDeclaration (_, _, modifiers, _, _)) = top_level_apex_class in
  let access_modifiers =
    List.filter ApexModifier.is_access_modifier modifiers
  in
  match access_modifiers with
  | [] -> None
  | [ h ] -> transpile_class_modifier top_level_apex_class h
  | _ ->
      raise
        (ModifierTranspilerException
           ("Too many access modifiers found", access_modifiers))

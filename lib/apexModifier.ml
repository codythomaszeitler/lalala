type modifierType =
  | Global
  | Public
  | Protected
  | Private
  | Transient
  | Static
  | Abstract
  | Final
  | Webservice
  | Override
  | Virtual
  | Testmethod
  | WithSharing
  | WithoutSharing
  | InheritedSharing

type modifier = { modifier_type : modifierType; loc : Location.location option }

let create ?(loc = None) (modifier_type : modifierType) : modifier =
  { modifier_type; loc }

let pr_modifier (ppf : Format.formatter) (modifier : modifier) : unit =
  let to_string (modifier_type : modifierType) : string =
    match modifier_type with
    | Global -> "global"
    | Public -> "public"
    | Protected -> "protected"
    | Private -> "private"
    | Transient -> "transient"
    | Static -> "static"
    | Abstract -> "abstract"
    | Final -> "final"
    | Webservice -> "webservice"
    | Override -> "override"
    | Virtual -> "virtual"
    | Testmethod -> "testmethod"
    | WithSharing -> "with_sharing"
    | WithoutSharing -> "without_sharing"
    | InheritedSharing -> "inherited_sharing"
  in
  match modifier with
  | { modifier_type; loc } ->
      Format.fprintf ppf
        "(Modifier@;<1 2>@[{modifier_type=\"%s\";@;<1 2>loc=%a}@]"
        (to_string modifier_type) Location.pr_location loc

type node = Identifier of string

let to_string (identifier : node) (depth : int) =
  match identifier with
  | Identifier name ->
      "(Identifier\n" ^ Formatter.tabs (depth + 1) ^ "(\"" ^ name ^ "\"))"

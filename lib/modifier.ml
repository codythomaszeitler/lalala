type node = Public | Annotation of Identifier.node

let to_string (modifier : node) (depth : int) =
  match modifier with
  | Public -> "(Public)"
  | Annotation identifer ->
      "(Annotation\n"
      ^ Formatter.tabs (depth + 1)
      ^ Identifier.to_string identifer (depth + 1)
      ^ ")"

let rec _to_strings (modifiers : node list) (depth : int) =
  match modifiers with
  | [] -> ""
  | h :: [] -> Formatter.tabs depth ^ to_string h depth
  | h :: t ->
      Formatter.tabs depth ^ to_string h depth ^ ";\n" ^ _to_strings t depth

let to_strings (modifiers : node list) (depth : int) =
  "[\n" ^ _to_strings modifiers (depth + 1) ^ "\n" ^ Formatter.tabs depth ^ "]"

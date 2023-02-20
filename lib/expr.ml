type node = Primary of Identifier.node

let to_string (expr : node) (depth : int) =
  match expr with
  | Primary identifier ->
      "(Primary\n"
      ^ Formatter.tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ "))"

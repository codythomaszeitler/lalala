type node = ApexType of Identifier.node

let to_string (apex_type : node) (depth : int) =
  match apex_type with
  | ApexType identifier ->
      "(ApexType\n"
      ^ Formatter.tabs (depth + 1)
      ^ Identifier.to_string identifier (depth + 1)
      ^ ")"

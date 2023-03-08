let rec filter_optionals optionals =
  let as_list_part optional =
    match optional with Some value -> [ value ] | None -> []
  in
  match optionals with
  | [] -> []
  | h :: t -> as_list_part h @ filter_optionals t

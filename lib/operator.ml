type operator =
  | Add of Location.location
  | Sub of Location.location
  | Mul of Location.location
  | Div of Location.location

let pr_operator (ppf : Format.formatter) (op : operator) : unit =
  match op with
  | Add location ->
      Format.fprintf ppf "@[<v 2>(Add{@;loc=%a@]})" Location.pr_location location
  | Sub location ->
      Format.fprintf ppf "@[<v 2>(Sub{@;loc=%a@]})" Location.pr_location location
  | Mul location ->
      Format.fprintf ppf "@[<v 2>(Mul{@;loc=%a@]})" Location.pr_location location
  | Div location ->
      Format.fprintf ppf "@[<v 2>(Div{@;loc=%a@]})" Location.pr_location location

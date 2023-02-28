type operator =
  | Add of Location.location
  | Sub of Location.location
  | Mul of Location.location
  | Div of Location.location

let pr_operator (ppf : Format.formatter) (op : operator) : unit =
  match op with
  | Add location ->
      Format.fprintf ppf "(Add{@[<v>@;loc=%a@]})" Location.pr_location location
  | Sub location ->
      Format.fprintf ppf "(Sub{@[<v>@;loc=%a@]})" Location.pr_location location
  | Mul location ->
      Format.fprintf ppf "(Mul{@[<v>@;loc=%a@]})" Location.pr_location location
  | Div location ->
      Format.fprintf ppf "(Div{@[<v>@;loc=%a@]})" Location.pr_location location

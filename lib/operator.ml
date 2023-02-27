type operator =
  | Add of Location.location
  | Sub of Location.location
  | Mul of Location.location
  | Div of Location.location

let pr_operator (ppf : Format.formatter) (op : operator) : unit =
  match op with
  | Add location ->
      Format.fprintf ppf "(Add@;<1 2>@[{loc=%a@])" Location.pr_location location
  | Sub location ->
      Format.fprintf ppf "(Sub@;<1 2>@[{loc=%a@])" Location.pr_location location
  | Mul location ->
      Format.fprintf ppf "(Mul@;<1 2>@[{loc=%a@])" Location.pr_location location
  | Div location ->
      Format.fprintf ppf "(Div@;<1 2>@[{loc=%a@])" Location.pr_location location

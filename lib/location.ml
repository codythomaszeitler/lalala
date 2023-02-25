type location = { line : int; row : int; column : int }

let create line row column : location = { line; row; column }

let pr_location (ppf : Format.formatter) (loc : location option) =
  match loc with
  | None -> Format.fprintf ppf "(Location)" 
  | Some location ->
      Format.fprintf ppf
        "(Location {@[ line=\"%d\" row=\"%d\" column=\"%d\" @]})" location.line
        location.row location.column

type location = { line : int; row : int; column : int }

let create line row column : location = { line; row; column }
let no_loc : location = { line = 0; row = 0; column = 0 }

let pr_location (ppf : Format.formatter) (loc : location) =
  if loc = no_loc then Format.fprintf ppf "(Location)"
  else
    Format.fprintf ppf
      "(Location{line=%d,row=%d,column=%d})" loc.line
      loc.row loc.column

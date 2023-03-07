type apexAnnotation =
  | IsTest of Location.location

let pr_apex_annotation (ppf : Format.formatter) (annotation : apexAnnotation) :
    unit =
  match annotation with
  | IsTest loc ->
      Format.fprintf ppf "@[<v 2>(IsTest{@;@[<v 2>@[<v 2>loc=%a@]})@]"
        Location.pr_location loc

type apexAnnotation = ApexAnnotation of Location.location * string

let pr_apex_annotation (ppf : Format.formatter) (annotation : apexAnnotation) :
    unit =
  match annotation with
  | ApexAnnotation (loc, name) ->
      Format.fprintf ppf
        "@[<v 2>(Annotation{@;@[<v 2>name=@;\"%s\"@]@;@[<v 2>loc=%a@]})@]" name
        Location.pr_location loc

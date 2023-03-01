type apexAnnotation = ApexAnnotation of Location.location * string

let pr_apex_annotation (ppf : Format.formatter) (annotation : apexAnnotation) :
    unit =
  match annotation with
  | ApexAnnotation (location, name) ->
      Format.fprintf ppf
        "[@<v 2>(Annotation{@;@[name=@;\"%s\"@] @[<v 2>location=@;%a@]})@]" name
        Location.pr_location location

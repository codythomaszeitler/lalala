type modifier =
  | Global of Location.location
  | Public of Location.location
  | Protected of Location.location
  | Private of Location.location
  | Transient of Location.location
  | Static of Location.location
  | Abstract of Location.location
  | Final of Location.location
  | Webservice of Location.location
  | Override of Location.location
  | Virtual of Location.location
  | Testmethod of Location.location
  | WithSharing of Location.location
  | WithoutSharing of Location.location
  | InheritedSharing of Location.location

let pr_modifier (ppf : Format.formatter) (modifier : modifier) : unit =
  match modifier with
  | Global location ->
      Format.fprintf ppf "@[<v 2>(Global{@;loc=%a}@]" Location.pr_location
        location
  | Public location ->
      Format.fprintf ppf "@[<v 2>(Public{@;loc=%a}@]" Location.pr_location
        location
  | Protected location ->
      Format.fprintf ppf "@[<v 2>(Protected{@;loc=%a}@]" Location.pr_location
        location
  | Private location ->
      Format.fprintf ppf "@[<v 2>(Private{@;loc=%a}@]" Location.pr_location
        location
  | Transient location ->
      Format.fprintf ppf "@[<v 2>(Transient{@;loc=%a}@]" Location.pr_location
        location
  | Static location ->
      Format.fprintf ppf "@[<v 2>(Static{@;loc=%a}@]" Location.pr_location
        location
  | Abstract location ->
      Format.fprintf ppf "@[<v 2>(Abstract{@;loc=%a}@]" Location.pr_location
        location
  | Final location ->
      Format.fprintf ppf "@[<v 2>(Final{@;loc=%a}@]" Location.pr_location
        location
  | Webservice location ->
      Format.fprintf ppf "@[<v 2>(Webservice{@;loc=%a}@]" Location.pr_location
        location
  | Override location ->
      Format.fprintf ppf "@[<v 2>(Override{@;loc=%a}@]" Location.pr_location
        location
  | Virtual location ->
      Format.fprintf ppf "@[<v 2>(Virtual{@;loc=%a}@]" Location.pr_location
        location
  | Testmethod location ->
      Format.fprintf ppf "@[<v 2>(Testmethod{@;loc=%a}@]" Location.pr_location
        location
  | WithSharing location ->
      Format.fprintf ppf "@[<v 2>(WithSharing{@;loc=%a}@]" Location.pr_location
        location
  | WithoutSharing location ->
      Format.fprintf ppf "@[<v 2>(WithoutSharing{@;loc=%a}@]"
        Location.pr_location location
  | InheritedSharing location ->
      Format.fprintf ppf "@[<v 2>(InheritedSharing{@;loc=%a}@]"
        Location.pr_location location

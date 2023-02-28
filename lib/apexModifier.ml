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
      Format.fprintf ppf "(Global@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Public location ->
      Format.fprintf ppf "(Public@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Protected location ->
      Format.fprintf ppf "(Protected@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Private location ->
      Format.fprintf ppf "(Private@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Transient location ->
      Format.fprintf ppf "(Transient@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Static location ->
      Format.fprintf ppf "(Static@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Abstract location ->
      Format.fprintf ppf "(Abstract@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Final location ->
      Format.fprintf ppf "(Final@[<v>{@;loc=%a}@]" Location.pr_location
        location
  | Webservice location ->
      Format.fprintf ppf "(Webservice@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Override location ->
      Format.fprintf ppf "(Webservice@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Virtual location ->
      Format.fprintf ppf "(Virtual@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | Testmethod location ->
      Format.fprintf ppf "(TestMethod@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | WithSharing location ->
      Format.fprintf ppf "(WithSharing@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | WithoutSharing location ->
      Format.fprintf ppf "(WithoutSharing@[<v>{@;loc=%a}@]"
        Location.pr_location location
  | InheritedSharing location ->
      Format.fprintf ppf "(InheritedSharing@[<v>{@;loc=%a}@]"
        Location.pr_location location

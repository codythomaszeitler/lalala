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
      Format.fprintf ppf "(Global@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Public location ->
      Format.fprintf ppf "(Public@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Protected location ->
      Format.fprintf ppf "(Protected@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Private location ->
      Format.fprintf ppf "(Private@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Transient location ->
      Format.fprintf ppf "(Transient@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Static location ->
      Format.fprintf ppf "(Static@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Abstract location ->
      Format.fprintf ppf "(Abstract@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Final location ->
      Format.fprintf ppf "(Final@;<1 2>@[{@;<1 2>loc=%a}@]" Location.pr_location
        location
  | Webservice location ->
      Format.fprintf ppf "(Webservice@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Override location ->
      Format.fprintf ppf "(Webservice@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Virtual location ->
      Format.fprintf ppf "(Virtual@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | Testmethod location ->
      Format.fprintf ppf "(TestMethod@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | WithSharing location ->
      Format.fprintf ppf "(WithSharing@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | WithoutSharing location ->
      Format.fprintf ppf "(WithoutSharing@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location
  | InheritedSharing location ->
      Format.fprintf ppf "(InheritedSharing@;<1 2>@[{@;<1 2>loc=%a}@]"
        Location.pr_location location

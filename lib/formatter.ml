
let rec tabs (num : int) = if num == 0 then "" else tabs (num - 1) ^ "  "
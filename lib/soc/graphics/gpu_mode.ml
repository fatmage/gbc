type t =
  | HBlank
  | VBlank
  | OAM_scan
  | Drawing_pixels

let of_int = function
  | 0 -> HBlank
  | 1 -> VBlank
  | 2 -> OAM_scan
  | 3 -> Drawing_pixels
  | _ -> failwith "Nonexistent rendering mode"

let to_int = function
  | HBlank         -> 0
  | VBlank         -> 1
  | OAM_scan       -> 2
  | Drawing_pixels -> 3
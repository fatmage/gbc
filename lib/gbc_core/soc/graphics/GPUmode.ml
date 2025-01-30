type t =
  | HBlank of int * int
  | VBlank of int
  | OAM_scan of int
  | Drawing_pixels of int * int

let of_int = function
  | 0 -> HBlank (0, 0)
  | 1 -> VBlank 0
  | 2 -> OAM_scan 0
  | 3 -> Drawing_pixels (0, 0)
  | _ -> failwith "Nonexistent rendering mode"

let to_int = function
  | HBlank (_, _) -> 0
  | VBlank _ -> 1
  | OAM_scan _ -> 2
  | Drawing_pixels (_, _) -> 3

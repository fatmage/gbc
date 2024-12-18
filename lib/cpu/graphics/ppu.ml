
type t = int
let empty : t = 0

let dot_of_mc mcycles speed =
  if speed then mcycles * 4 else mcycles * 2

let process_ppu ppu gmem dots = ppu
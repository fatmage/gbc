module OAM = Ram.RAM

module VRAM = struct
  module Bank = (val Ram.make_chunk 8912 0x8000)

  type t = Bank.t * Bank.t * int

  let empty = Bank.empty, Bank.empty, 0

  let get m i =
    match m, i with
    | (_,_,bank), 0xFF4F -> bank lor 0b11111110
    | (m,_,_), i         -> Bank.get m i

  let set m i v =
    match m, i with
    | (b1, b2, bank), 0xFF4F ->
      if v land 1 = bank then m else (b2, b1, v land 1)
    | (b1 ,b2, bank), i -> (Bank.set b1 i v, b2, bank)

  let in_range i = Bank.in_range i || i = 0xFF4F

end

module LCD_Control = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF40 <= i && i <= 0xFF4B
end

module Palettes = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF68 <= i && i <= 0xFF6B
end

type t = { vram : VRAM.t; oam : OAM.t; lcdc : LCD_Control.t; palettes : Palettes.t }

let empty = { vram = VRAM.empty; oam = OAM.empty; lcdc = LCD_Control.empty; palettes = Palettes.empty }
let get t _ = 0
let set t _ _ = t
let in_range i = VRAM.in_range i || OAM.in_range i || LCD_Control.in_range i || Palettes.in_range i


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
  type t =
    {
      (* DMG registers *)
      bgp : int; obp0 : int; obp1 : int;
      (* CGB registers + palette memory *)
      bcps: int; bcpd : int; ocps: int; ocpd: int;
    }
  let empty =
    {
      bgp = 0; obp0 = 0; obp1 = 0;
      bcps = 0; bcpd = 0; ocps = 0; ocpd = 0;
    }

  let get m =
    function
    | 0xFF47 -> m.bgp
    | 0xFF48 -> m.obp0
    | 0xFF49 -> m.obp1
    | 0xFF68 -> m.bcps
    | 0xFF69 -> m.bcpd
    | 0xFF6A -> m.ocps
    | 0xFF6B -> m.ocpd
  let set m i v =
    match i with
    | 0xFF47 -> { m with bgp  = v }
    | 0xFF48 -> { m with obp0 = v }
    | 0xFF49 -> { m with obp1 = v }
    | 0xFF68 -> { m with bcps = v }
    | 0xFF69 -> { m with bcpd = v }
    | 0xFF6A -> { m with ocps = v }
    | 0xFF6B -> { m with ocpd = v }

  let in_range i = (0xFF47 <= i && i <= 0xFF48) || (0xFF68 <= i && i <= 0xFF6B)
end

type t = { mode : Gpu_mode.t; vram : VRAM.t; oam : OAM.t; lcdc : LCD_Control.t; palettes : Palettes.t }

let empty = { mode = OAM_scan; vram = VRAM.empty; oam = OAM.empty; lcdc = LCD_Control.empty; palettes = Palettes.empty }
let get t _ = 0
let set t _ _ = t
let in_range i = VRAM.in_range i || OAM.in_range i || LCD_Control.in_range i || Palettes.in_range i

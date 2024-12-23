module OAM = RAM.RAM

module VRAM = struct
  module Bank = (val RAM.make_chunk 8912 0x8000)

  type t = Bank.t * Bank.t * int

  let initial = Bank.initial, Bank.initial, 0

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

module LCD_Regs = struct
  type t =
    {
      lcdc : int; ly : int; lyc : int; dma : int;
      stat : int; scy: int; scx : int; wy : int; wx : int;
    }

  let initial =
    { lcdc = 0; ly = 0; lyc = 0; dma = 0;
      stat = 0; scy = 0; scx = 0; wy = 0; wx = 0 }

  let get m =
    function
    | 0xFF40 -> m.lcdc
    | 0xFF41 -> m.stat
    | 0xFF42 -> m.scy
    | 0xFF43 -> m.scx
    | 0xFF44 -> m.ly
    | 0xFF45 -> m.lyc
    | 0xFF46 -> m.dma
    | 0xFF4A -> m.wy
    | 0xFF4B -> m.wx

  let set m i v =
    match i with
    | 0xFF40 -> { m with lcdc = v }
    | 0xFF41 -> { m with stat = v land 0xF8 lor m.stat }
    | 0xFF42 -> { m with scy = v }
    | 0xFF43 -> { m with scx = v }
    | 0xFF44 -> m
    | 0xFF45 -> { m with lyc = v }
    | 0xFF46 -> { m with dma = v }
    | 0xFF4A -> { m with wy = v }
    | 0xFF4B -> { m with wx = v }

  let lcd_enabled { lcdc; _ } = lcdc land 0x80 = 1
  let window_tm_area { lcdc; _ } = if lcdc land 0x40 = 1 then 0x9C00 else 0x9800
  let window_enabled { lcdc; _ } = lcdc land 0x20 = 1
  let bw_base_pointer { lcdc; _ } = if lcdc land 0x10 = 1 then 0x8000 else 0x8800
  let bg_tm_area { lcdc; _ } = if lcdc land 0x08 = 1 then 0x9C00 else 0x9800
  let obj_size { lcdc; _ } = lcdc land 0x04 = 1
  let obj_enabled { lcdc; _ } = lcdc land 0x02 = 1
  let bgwindow_ep { lcdc; _ } = lcdc land 0x01 = 1

  let cmp_lyc m = if m.ly = m.lyc then { m with stat = m.stat lor 0x04 } else m
  let in_range i = (0xFF40 <= i && i <= 0xFF46) || i = 0xFF4A || i = 0xFF4B
end


module Palettes = struct
  type t =
    {
      (* DMG registers *)
      bgp : int; obp0 : int; obp1 : int;
      (* CGB registers + palette memory *)
      bcps: int; bcpd : int; ocps: int; ocpd: int;
      cram : int list (* length 64 *)
    }

  let initial =
    {
      bgp = 0; obp0 = 0; obp1 = 0;
      bcps = 0; bcpd = 0; ocps = 0; ocpd = 0;
      cram = List.init 64  (fun _ -> 0)
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

type t = { mode : GPUmode.t; vram : VRAM.t; oam : OAM.t; lcd_regs : LCD_Regs.t; palettes : Palettes.t }

let initial = { mode = OAM_scan; vram = VRAM.initial; oam = OAM.initial; lcd_regs = LCD_Regs.initial; palettes = Palettes.initial }
let get t =
  function
  | i when VRAM.in_range i -> VRAM.get t.vram i
  | i when OAM.in_range i -> OAM.get t.oam i
  | i when LCD_Regs.in_range i -> LCD_Regs.get t.lcd_regs i
  | i when Palettes.in_range i -> Palettes.get t.palettes i

let set t i v =
  match i with
  | _ when VRAM.in_range i -> { t with vram = VRAM.set t.vram i v }
  | _ when OAM.in_range i -> { t with oam = OAM.set t.oam i v }
  | _ when LCD_Regs.in_range i -> { t with lcd_regs = LCD_Regs.set t.lcd_regs i v }
  | _ when Palettes.in_range i -> { t with palettes = Palettes.set t.palettes i v }

let in_range i = VRAM.in_range i || OAM.in_range i || LCD_Regs.in_range i || Palettes.in_range i

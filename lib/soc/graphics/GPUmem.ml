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

  let get_tile_index (b1, b2, bank) area y x =
    let bank0 = if bank = 0 then b1 else b2 in
    Bank.get bank0 (area + ((y/8) * 32) + (x/8))

  let get_tile_attributes (b1, b2, bank) area y x =
    let bank1 = if bank = 0 then b2 else b1 in
    Bank.get bank1 (area + ((y/8) * 32) + (x/8))

  let get_tile_data_row m area index row =
    match area with
    | 0x8000 -> (get m (index * 16 + row * 2)), (get m (index * 16 + row * 2 + 1))
    | 0x9000 ->
      let s_index = (index land 0x7F) - (index land 0x80) in
      (get m (s_index * 16)), (get m (index * 16 + 1))

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
    | 0xFF4A -> m.wy
    | 0xFF4B -> m.wx

  let set m i v =
    match i with
    | 0xFF40 -> { m with lcdc = v }
    | 0xFF41 -> { m with stat = (v land 0xF8) lor (m.stat land 0x7) }
    | 0xFF42 -> { m with scy = v }
    | 0xFF43 -> { m with scx = v }
    | 0xFF44 -> m
    | 0xFF45 -> { m with lyc = v }
    | 0xFF46 -> { m with dma = v }
    | 0xFF4A -> { m with wy = v }
    | 0xFF4B -> { m with wx = v }

  (* LCDC *)
  let lcd_enabled { lcdc; _ } = lcdc land 0x80 > 0
  let window_tm_area { lcdc; _ } = if lcdc land 0x40 > 0 then 0x9C00 else 0x9800
  let window_enabled { lcdc; _ } = lcdc land 0x20 > 0
  let bw_base_pointer { lcdc; _ } = if lcdc land 0x10 > 0 then 0x8000 else 0x9000
  let bg_tm_area { lcdc; _ } = if lcdc land 0x08 > 0 then 0x9C00 else 0x9800
  let obj_size { lcdc; _ } = if lcdc land 0x04 > 0 then 16 else 8
  let obj_enabled { lcdc; _ } = lcdc land 0x02 > 0
  let bgwindow_ep { lcdc; _ } = lcdc land 0x01 > 0

  (* STAT *)
  let lyc_cond { stat; _ } = stat land 0x40 > 0
  let mode2_cond { stat; _ } = stat land 0x20 > 0
  let mode1_cond { stat; _ } = stat land 0x10 > 0
  let mode0_cond { stat; _ } = stat land 0x08 > 0
  let lyc_ly_eq { stat; _ } = stat land 0x04 > 0
  let get_mode { stat; _ } = stat land 0x03 |> GPUmode.of_int
  let set_mode m mode = match m with  { stat; _ } -> { m with stat = (stat land 0xF8) lor mode }


  let inc_ly m =
    let ly = m.ly + 1 in
    if ly >= 160 then  { m with ly = 0 }  else { m with ly }
  let cmp_lyc m =
    if m.ly = m.lyc then
      { m with stat = m.stat lor 0x04 }, true
    else
      { m with stat = m.stat land 0xBF }, false
  let in_range i = (0xFF40 <= i && i <= 0xFF45) || i = 0xFF4A || i = 0xFF4B
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
let initial = { mode = OAM_scan 0; vram = VRAM.initial; oam = OAM.initial; lcd_regs = LCD_Regs.initial; palettes = Palettes.initial }
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

let get_mode m = m.mode

let inc_ly m = { m with lcd_regs = LCD_Regs.inc_ly m.lcd_regs }


let update_mode m mode = { m with mode }
let change_mode m mode =
  match mode with
  | GPUmode.HBlank (_,_)         -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 0 }
  | GPUmode.VBlank _             -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 1 }
  | GPUmode.OAM_scan _           -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 2 }
  | GPUmode.Drawing_pixels (_,_) -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 3 }

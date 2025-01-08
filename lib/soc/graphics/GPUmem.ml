module type Palettes_intf = sig
  include Addressable.S

  val lookup_bgw : t -> int -> int -> int
  val lookup_obj : t -> int -> int -> int
end


module type S = sig

  module OAM : sig
  include Addressable.S
  end
  module VRAM : sig
    include RAM.S
    val get_tile_index : t -> int -> int -> int -> int
    val get_tile_attributes : t -> int -> int -> int -> int
    val get_tile_data_row : t -> int -> int -> int -> int -> int * int
  end
  module LCD_Regs : sig

    type t =
    {
      lcdc : int; ly : int; lyc : int; dma : int;
      stat : int; scy: int; scx : int; wy : int; wx : int;
    }
    val initial : t
    val get : t -> int -> int
    val set : t -> int -> int -> t
    val in_range : int -> bool

    (* LCDC *)
    val lcd_enabled : t -> bool
    val window_tm_area : t -> int
    val window_enabled : t -> bool
    val bw_base_pointer : t -> int
    val bg_tm_area : t -> int
    val obj_size : t -> int
    val obj_enabled : t -> bool
    val bgwindow_ep : t -> bool

    (* STAT *)
    val lyc_cond : t -> bool
    val mode2_cond : t -> bool
    val mode1_cond : t -> bool
    val mode0_cond : t -> bool
    val lyc_ly_eq : t -> bool
    val get_mode : t -> GPUmode.t
    val set_mode : t -> int -> t

    val inc_ly : t -> t
    val cmp_lyc : t -> t * bool

  end
  module Palettes : Palettes_intf

  type t = { mode : GPUmode.t; vram : VRAM.t; oam : OAM.t; lcd_regs : LCD_Regs.t; palettes : Palettes.t }
  val initial : t
  val get : t -> int -> int
  val set : t -> int -> int -> t

  val in_range : int -> bool

  val get_mode : t -> GPUmode.t
  val inc_ly : t -> t
  val reset_ly : t -> t
  val get_ly : t -> int

  val update_mode : t -> GPUmode.t -> t

  val change_mode : t -> GPUmode.t -> t

end

module VRAMBank = (val RAM.make_chunk 8912 0x8000)

module Palettes_CGB : Palettes_intf = struct
  type t =
    {
      (* DMG registers *)
      bgp : int; obp0 : int; obp1 : int;
      (* CGB registers + palette memory *)
      bcps: int; (* bcpd : int; *) ocps: int; (* ocpd: int; *)
      obj_cram : int list; (* length 64 *)
      bgw_cram : int list; (* length 64 *)
    }

  let initial =
    {
      bgp = 0; obp0 = 0; obp1 = 0;
      bcps = 0; ocps = 0;
      obj_cram = List.init 64  (fun _ -> 0);
      bgw_cram = List.init 64  (fun _ -> 0)
    }

  let get m =
    function
    | 0xFF47 -> m.bgp
    | 0xFF48 -> m.obp0
    | 0xFF49 -> m.obp1
    | 0xFF68 -> m.bcps
    | 0xFF69 -> List.nth m.bgw_cram @@ m.bcps land 0x3F
    | 0xFF6A -> m.ocps
    | 0xFF6B -> List.nth m.obj_cram @@ m.ocps land 0x3F

  let set m i v =
    match i with
    | 0xFF47 -> { m with bgp  = v }
    | 0xFF48 -> { m with obp0 = v }
    | 0xFF49 -> { m with obp1 = v }
    | 0xFF68 -> { m with bcps = v }
    | 0xFF69 ->
      let addr = m.bcps land 0x3F in
      let m = if m.bcps land 0x80 > 0 then { m with bcps = m.bcps + 1 land 0x3F } else m in
      let bgw_cram = List.mapi (fun i ei -> if i = addr then v else ei) m.bgw_cram in
      { m with bgw_cram }
    | 0xFF6A -> { m with ocps = v }
    | 0xFF6B ->
      let addr = m.ocps land 0x3F in
      let m = if m.ocps land 0x80 > 0 then { m with ocps = m.ocps + 1 land 0x3F } else m in
      let obj_cram = List.mapi (fun i ei -> if i = addr then v else ei) m.obj_cram in
      { m with obj_cram }

  let rec nth_2 xs i =
    match xs,i with
    | l::h::xs, 0 -> l lsl 8 lor h
    | x::xs, i -> nth_2 xs (i-1)
  let lookup_bgw m palette color = nth_2 m.bgw_cram (palette * 8 + (color * 2))

  let lookup_obj m palette color = nth_2 m.bgw_cram (palette * 8 + (color * 2))

  let in_range i = (0xFF47 <= i && i <= 0xFF48) || (0xFF68 <= i && i <= 0xFF6B)
end



module Make (M : Palettes_intf) : S = struct

  module Palettes = M

  (* TODO *)
  module OAM = struct

    type object_data = { y_p : int; x_p : int; t_index : int; flags : int }

    let obj_empty = { y_p = 0; x_p = 0; t_index = 0; flags = 0 }

    type t = object_data list
    let initial = List.init 40 (fun _ -> obj_empty )
    let get xs i =
      let obj_i = i / 4 in
      let in_obj = i mod 4 in
      let { y_p; x_p; t_index; flags} = List.nth xs obj_i in
      match in_obj with
      | 0 -> y_p
      | 1 -> x_p
      | 2 -> t_index
      | 3 -> flags
    let set xs i v =
      let obj_i = i / 4 in
      let in_obj = i mod 4 in
      let rec aux xs i v acc =
        match xs, i with
        | x::xs, 0 ->
          let new_x =
            begin match in_obj with
            | 0 -> { x with y_p = v }
            | 1 -> { x with x_p = v }
            | 2 -> { x with t_index = v }
            | 3 -> { x with flags = v }
            end
          in (List.rev acc) @ (new_x :: xs)
        | x::xs, i -> aux xs (i - 1) v (x::acc)
      in
      aux xs obj_i v []

    let in_range i = i >= 0xFE00 && i <= 0xFE9F
  end

  module VRAM = struct
    module Bank = VRAMBank

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

    let get_tile_data_row (b1, b2, b) area index row chosen_bank =
      let bank =
        match b, chosen_bank with
        | 0, 0 -> b1
        | 0, 1 -> b2
        | 1, 0 -> b2
        | 1, 1 -> b1 in
      match area with
      | 0x8000 -> (Bank.get bank (index * 16 + row * 2)), (Bank.get bank (index * 16 + row * 2 + 1))
      | 0x9000 ->
        let s_index = (index land 0x7F) - (index land 0x80) in
        (Bank.get bank (s_index * 16)), (Bank.get bank (index * 16 + 1))

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

  let reset_ly m = { m with lcd_regs = { m.lcd_regs with ly = 0 } }
  let get_ly m = m.lcd_regs.ly


  let update_mode m mode = { m with mode }
  let change_mode m mode =
    match mode with
    | GPUmode.HBlank (_,_)         -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 0 }
    | GPUmode.VBlank _             -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 1 }
    | GPUmode.OAM_scan _           -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 2 }
    | GPUmode.Drawing_pixels (_,_) -> { m with mode; lcd_regs = LCD_Regs.set_mode m.lcd_regs 3 }
end
module FIFO = struct

  type 'a t = 'a list * 'a list

  let empty = [], []

  let head =
    function
    | [], _    -> failwith "head error: empty fifo"
    | x::xs, _ -> x

  let head_opt =
    function
    | [], _    -> None
    | x::xs, _ -> Some x

  let push (f,t) v = f,v::t

  let pop =
    function
    | x::f, t -> f, t
    | [], t   -> List.rev t, []

end

module type S = sig
  type state

  val framebuffer : int array array

  type t
  type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : int; sprite_buffer : int list }

  val screen_w : int
  val screen_h : int
  val bg_wh : int
  val window_wh : int
  val line_duration : int

  val initial : t
  val dot_of_mc : int -> bool -> int

  val process_ppu : state -> t -> int -> state * t
end

module Make (State : State.S) : (S with type state = State.t) = struct

  type state = State.t

  let screen_w = 160
  let screen_h = 144
  let bg_wh = 256
  let window_wh = 256
  let line_duration = 456

  let cgb_mode = true

  let framebuffer = Array.make_matrix 144 160 0

  type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : int; sprite_buffer : int list }

  type t = { bg_fifo : pixel FIFO.t; obj_fifo : pixel FIFO.t }
  let initial = { bg_fifo = FIFO.empty; obj_fifo = FIFO.empty }

  let dot_of_mc mcycles speed =
    if speed then mcycles * 4 else mcycles * 2

  (* MAYBE TODO - actual oam scan *)
  let scan_oam ppu st = ppu

  (* TODO - actual line rendering *)

  (* let render_bgw_line (st : state) ppu ly =
    let render_bg_line t ly tile_data_area =
      let scy, scx = st.gpu_mem.lcd_regs.scy, st.gpu_mem.lcd_regs.scx in
      let y = (scy + ly) mod bg_wh in
      let bg_tile_map_area = State.GPUmem.LCD_Regs.bg_tm_area st.gpu_mem.lcd_regs in
      let row_in_tile = y mod 8 in
      let lx = ref 0 in
      while !lx < screen_w do
        let x = (scx + !lx) mod bg_wh in
        let col_in_tile = x mod 8 in
        let tile_index = State.GPUmem.VRAM.get_tile_index st.gpu_mem.vram bg_tile_map_area y x in
        let p1, p2 = State.GPUmem.VRAM.get_tile_data_row st.gpu_mem.vram tile_data_area tile_data_area row_in_tile in
        let len =
          if col_in_tile > 0 then
            8 - col_in_tile
        else if screen_w - !lx < 8 then
          screen_w - !lx
        else
          8
        in
        for i = 0 to len -1 do
          let color = State.GPUmem.Palettes.lookup st.gpu_mem.palettes  *)


  let render_line st ppu = ()


  let check_ly_lyc (st : state) =
    let lcd_regs, interrupt = State.GPUmem.LCD_Regs.cmp_lyc st.gpu_mem.lcd_regs in
    if State.GPUmem.LCD_Regs.lyc_cond st.gpu_mem.lcd_regs && interrupt then
      State.request_LCD st
    else
      st

  let process_ppu (st : state) ppu dots =
    match State.GPUmem.get_mode st.gpu_mem with
    | GPUmode.HBlank (c, m)         ->
      let new_c = c + dots in
      if new_c >= m then
        let st = st |> State.inc_ly |> check_ly_lyc in
        if st.gpu_mem.lcd_regs.ly < screen_h then
          let st = if State.GPUmem.LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          State.change_mode st @@ OAM_scan (new_c - m), ppu
        else
          let st = if State.GPUmem.LCD_Regs.mode1_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          let st = State.request_VBlank st in
          State.change_mode st @@ VBlank (new_c - m), ppu
      else
        State.update_mode st @@ HBlank (new_c, m), ppu
    | GPUmode.VBlank c       ->
      let new_c = c + dots in
      if new_c >= line_duration then
        let st = st |> State.inc_ly |> check_ly_lyc in
        if State.get_ly st < screen_h + 10 then
          State.update_mode st @@ VBlank (new_c - line_duration), ppu
        else
          let st = st |> State.reset_ly |> check_ly_lyc in
          let st = if State.GPUmem.LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          State.change_mode st @@ OAM_scan (new_c - line_duration), ppu
      else
        State.update_mode st @@ VBlank new_c, ppu
    | GPUmode.OAM_scan c    ->
      let new_c = c + dots in
      if new_c >= 80 then
        State.change_mode st @@ Drawing_pixels (new_c - 80, 172), scan_oam ppu st
      else
        State.update_mode st @@ OAM_scan new_c, ppu
    | GPUmode.Drawing_pixels (c, m) ->
      let new_c = c + dots in
      if new_c >= m then
        let _ = render_line st ppu in
        let st = if State.GPUmem.LCD_Regs.mode0_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
        State.change_mode st @@ HBlank (new_c, 204), ppu
      else
        State.update_mode st @@ Drawing_pixels (new_c, m), ppu

end

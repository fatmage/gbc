
module type S = sig
  type state

  type t
  type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : bool }

  val framebuffer : int array array
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

  type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : bool }
  let empty_pixel = { color = 0; palette = 0; sprite_prio = 0; bg_prio = false }

  let mk_pixel color palette sprite_prio bg_prio = {color; palette; sprite_prio; bg_prio }


  let framebuffer = Array.make_matrix screen_w screen_h 0
  let bgw_buffer = Array.make screen_w empty_pixel
  let obj_buffer = Array.make screen_w empty_pixel


  type t = { sprite_buffer : int list }
  let initial = { sprite_buffer = [] }

  let dot_of_mc mcycles speed =
    if speed then mcycles * 4 else mcycles * 2

  let rev_u8 u8 =
    let rec loop acc u8 =
      function
      | 0 -> acc
      | n -> loop ((acc lsl 1) lor (u8 land 1)) (u8 lsr 1) (n-1) in
      loop 0 u8 8

  let render_bgw_line (st : state) ppu ly =
    let render_bg_line (st : state) ly tile_data_area =
      let scy, scx = st.gpu_mem.lcd_regs.scy, st.gpu_mem.lcd_regs.scx in
      let y = (scy + ly) mod bg_wh in
      let bg_tile_map_area = State.GPUmem.LCD_Regs.bg_tm_area st.gpu_mem.lcd_regs in
      let row_in_tile = y mod 8 in
      let lx = ref 0 in
      while !lx < screen_w do
        let x = (scx + !lx) mod bg_wh in
        let col_in_tile = x mod 8 in
        let tile_index = State.GPUmem.VRAM.get_tile_index st.gpu_mem.vram bg_tile_map_area y x in
        let tile_attr = State.GPUmem.VRAM.get_tile_attributes st.gpu_mem.vram bg_tile_map_area y x in
        let prio = tile_attr land 0x80 > 0 in
        let y_flip = tile_attr land 0x40 > 0 in
        let x_flip = tile_attr land 0x20 > 0 in
        let bank = tile_attr land 0x08 lsr 3 in
        let palette = tile_attr land 0x07 in
        let row_in_tile = if y_flip then 8 - row_in_tile else row_in_tile in
        let p1, p2 = State.GPUmem.VRAM.get_tile_data_row st.gpu_mem.vram tile_data_area tile_index row_in_tile bank in
        let p1 = if x_flip then ref (rev_u8 p1) else ref p1 in
        let p2 = if x_flip then ref (rev_u8 p2) else ref p2 in
        let len =
          if col_in_tile > 0 then
            8 - col_in_tile
        else if screen_w - !lx < 8 then
          screen_w - !lx
        else
          8
        in
        for i = 0 to len - 1 do
          let color = (!p1 land 0b1) lor ((!p2 land 0b1) lsl 1) in
          p1 := !p1 lsr 1;
          p2 := !p2 lsr 1;
          bgw_buffer.(i) <- (mk_pixel color palette 0 prio)
        done;
        lx := !lx + len
      done
    in
    let render_w_line (st : state) ly tile_data_area =
      let wy = st.gpu_mem.lcd_regs.wy in
      let wx = st.gpu_mem.lcd_regs.wx in
      if wy <= ly && ly <= wy + window_wh && wx <= screen_w then
        let window_tile_map_area = State.GPUmem.LCD_Regs.window_tm_area st.gpu_mem.lcd_regs in
        let y_in_w = Int.abs (ly - wy) in
        let row_in_tile = y_in_w mod 8 in
        let lx = ref (if wx < 0 then 0 else wx) in
        while !lx < screen_w do
          let x_in_w = Int.abs (!lx - wx) in
          let tile_index = State.GPUmem.VRAM.get_tile_index st.gpu_mem.vram window_tile_map_area y_in_w x_in_w in
          let tile_attr = State.GPUmem.VRAM.get_tile_attributes st.gpu_mem.vram window_tile_map_area y_in_w x_in_w in
          let prio = tile_attr land 0x80 > 0 in
          let y_flip = tile_attr land 0x40 > 0 in
          let x_flip = tile_attr land 0x20 > 0 in
          let bank = tile_attr land 0x08 lsr 3 in
          let palette = tile_attr land 0x07 in
          let row_in_tile = if y_flip then 8 - row_in_tile else row_in_tile in
          let p1, p2 = State.GPUmem.VRAM.get_tile_data_row st.gpu_mem.vram tile_data_area tile_index row_in_tile bank in
          let p1 = if x_flip then ref (rev_u8 p1) else ref p1 in
          let p2 = if x_flip then ref (rev_u8 p2) else ref p2 in
          let len = if screen_w - !lx < 8 then screen_w - !lx else 8 in
          for i = 0 to len - 1 do
            let color = (!p1 land 0b1) lor ((!p2 land 0b1) lsl 1) in
            p1 := !p1 lsr 1;
            p2 := !p2 lsr 1;
            bgw_buffer.(i) <- (mk_pixel color palette 0 prio)
          done;
          lx := !lx + len
        done
    in
    let tile_data_area = State.GPUmem.LCD_Regs.bw_base_pointer st.gpu_mem.lcd_regs in
    render_bg_line st ly tile_data_area;
    if State.GPUmem.LCD_Regs.window_enabled st.gpu_mem.lcd_regs then
      render_w_line st ly tile_data_area

  (* MAYBE TODO - actual oam scan *)
  let scan_oam (st : state) ppu = ppu

  (* TODO render_obj_line *)
  let render_obj_line st ppu ly = ()

  let render_line (st : state) ppu =
    let ly = st.gpu_mem.lcd_regs.ly in
    render_bgw_line st ppu ly;
    if State.GPUmem.LCD_Regs.obj_enabled st.gpu_mem.lcd_regs then
      render_obj_line st ppu ly;
    (* TODO: mix bgw and obj linebuffer to framebuffer *)
    ()


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
        State.change_mode st @@ Drawing_pixels (new_c - 80, 172), scan_oam st ppu
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

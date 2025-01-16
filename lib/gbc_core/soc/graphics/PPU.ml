
module type S = sig
  type state
  type pixel = { color: int; palette : int; sprite_prio : int; prio : bool }

  val framebuffer : int array array
  val screen_w : int
  val screen_h : int
  val bg_wh : int
  val window_wh : int
  val line_duration : int

  val dot_of_mc : int -> bool -> int

  val process_ppu : state -> int -> state
end

module Make (State : State.S) : (S with type state = State.t) = struct

  type state = State.t

  let screen_w = 160
  let screen_h = 144
  let bg_wh = 256
  let window_wh = 256
  let line_duration = 456


  type pixel = { color: int; palette : int; sprite_prio : int; prio : bool }
  let empty_pixel = { color = -1; palette = 0; sprite_prio = 0; prio = false }

  let mk_pixel color palette sprite_prio prio = {color; palette; sprite_prio; prio }


  let framebuffer = Array.make_matrix screen_w screen_h 0
  let bgw_buffer = Array.make screen_w empty_pixel
  let obj_buffer = Array.make screen_w empty_pixel

  type obj_data = State.GPUmem.scanned_obj
  let sprite_buffer : obj_data list ref = ref []

  let reset_sprite_buffer () = sprite_buffer := []

  let dot_of_mc mcycles speed =
    if speed then mcycles * 4 else mcycles * 2

  let render_bgw_line (st : state)  ly =
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
        let p1 = if x_flip then ref (Utils.rev_u8 p1) else ref p1 in
        let p2 = if x_flip then ref (Utils.rev_u8 p2) else ref p2 in
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
          let p1 = if x_flip then ref (Utils.rev_u8 p1) else ref p1 in
          let p2 = if x_flip then ref (Utils.rev_u8 p2) else ref p2 in
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




  let scan_oam (st : state) = sprite_buffer := State.GPUmem.scan_oam st.gpu_mem st.gpu_mem.lcd_regs.ly

  (* TODO render_obj_line *)
  (* We have all objects in sprite_buffer, we need to iter through the list
     and write every obj to pixel_buffer like in obj and window *)
  let render_obj_line () =
    let draw_obj obj_prio ({ x_p; p1; p2 ; palette; prio } : obj_data) =
      let lx = ref (x_p - 8) in
      let p1 = ref p1 in
      let p2 = ref p2 in
      for _ = 0 to 7 do
        begin
        if !lx >= 0 && !lx < 160 then
          let color = (!p1 land 0b1) lor ((!p2 land 0b1) lsl 1) in
          obj_buffer.(!lx) <- (mk_pixel color palette obj_prio prio)
        end;
        p1 := !p1 lsr 1;
        p2 := !p2 lsr 1;
      done
    in
    List.iteri draw_obj !sprite_buffer

  let push_pixel x y arr palette color =
    framebuffer.(x).(y) <- State.GPUmem.Palettes.lookup_arr arr palette color

  let render_line (st : state) =
    let bgw_palette = State.GPUmem.Palettes.bgw_array st.gpu_mem.palettes in
    let obj_palette = State.GPUmem.Palettes.obj_array st.gpu_mem.palettes in
    let ly = st.gpu_mem.lcd_regs.ly in
    render_bgw_line st ly;
    begin
    if State.GPUmem.LCD_Regs.obj_enabled st.gpu_mem.lcd_regs then
      render_obj_line ()
    end;
    for i = 0 to 159 do
      match bgw_buffer.(i), obj_buffer.(i), State.GPUmem.LCD_Regs.bgwindow_ep st.gpu_mem.lcd_regs with
      (* No object pixel *)
      | { color; palette; _}, {color = -1; _}, _ ->
        push_pixel i ly bgw_palette palette color
      (* Transparent object *)
      | { color; palette; _}, {color = 0; _}, _ ->
        push_pixel i ly bgw_palette palette color
      | _, {palette; color; _}, false
      | {prio = false; _}, {palette; color; prio = false; _}, true ->
        push_pixel i ly obj_palette palette color
      | {color = 0; _}, {palette; color; _}, true ->
        push_pixel i ly obj_palette palette color
      | {palette; color; _}, _, true ->
        push_pixel i ly bgw_palette palette color
    done;
    reset_sprite_buffer ()




  let check_ly_lyc (st : state) =
    let lcd_regs, interrupt = State.GPUmem.LCD_Regs.cmp_lyc st.gpu_mem.lcd_regs in
    let st = { st with gpu_mem = { st.gpu_mem with lcd_regs } } in
    if State.GPUmem.LCD_Regs.lyc_cond st.gpu_mem.lcd_regs && interrupt then
      State.request_LCD st
    else
      st

  let process_ppu (st : state) dots =
    match State.GPUmem.get_mode st.gpu_mem with
    | GPUmode.HBlank (c, m)         ->
      let new_c = c + dots in
      if new_c >= m then
        let st = st |> State.inc_ly |> check_ly_lyc in
        if st.gpu_mem.lcd_regs.ly < screen_h then
          let st = if State.GPUmem.LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          State.change_mode st @@ OAM_scan (new_c - m)
        else
          let st = if State.GPUmem.LCD_Regs.mode1_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          let st = State.request_VBlank st in
          State.change_mode st @@ VBlank (new_c - m)
      else
        State.update_mode st @@ HBlank (new_c, m)
    | GPUmode.VBlank c       ->
      let new_c = c + dots in
      if new_c >= line_duration then
        let st = st |> State.inc_ly |> check_ly_lyc in
        if State.get_ly st < screen_h + 10 then
          State.update_mode st @@ VBlank (new_c - line_duration)
        else
          let st = st |> State.reset_ly |> check_ly_lyc in
          let st = if State.GPUmem.LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
          State.change_mode st @@ OAM_scan (new_c - line_duration)
      else
        State.update_mode st @@ VBlank new_c
    | GPUmode.OAM_scan c    ->
      let new_c = c + dots in
      if new_c >= 80 then
        let _ = scan_oam st in
        State.change_mode st @@ Drawing_pixels (new_c - 80, 172)
      else
        State.update_mode st @@ OAM_scan new_c
    | GPUmode.Drawing_pixels (c, m) ->
      let new_c = c + dots in
      if new_c >= m then
        let _ = render_line st in
        let st = if State.GPUmem.LCD_Regs.mode0_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
        State.change_mode st @@ HBlank (new_c, 204)
      else
        State.update_mode st @@ Drawing_pixels (new_c, m)

end

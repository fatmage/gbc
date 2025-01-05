open State
open GPUmem

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

let screen_w = 160
let screen_h = 144
let bg_wh = 256
let window_wh = 256
let line_duration = 456

let ref framebuffer = Array.make_matrix 144 160 0

type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : int; sprite_buffer : int list }

type t = { bg_fifo : pixel FIFO.t; obj_fifo : pixel FIFO.t }
let initial = { bg_fifo = FIFO.empty; obj_fifo = FIFO.empty }

let dot_of_mc mcycles speed =
  if speed then mcycles * 4 else mcycles * 2

(* MAYBE TODO - actual oam scan *)
let scan_oam ppu st = ppu

(* TODO - actual line rendering *)
let render_line st ppu = ()


let reset_ly st = { st with gpu_mem = { st.gpu_mem with lcd_regs = { st.gpu_mem.lcd_regs with ly = 0 } } }

let check_ly_lyc st =
  let lcd_regs, interrupt = LCD_Regs.cmp_lyc st.gpu_mem.lcd_regs in
  if LCD_Regs.lyc_cond st.gpu_mem.lcd_regs && interrupt then
    request_LCD st
  else
    st

let process_ppu st ppu dots =
  match GPUmem.get_mode st.gpu_mem with
  | GPUmode.HBlank (c, m)         ->
    let new_c = c + dots in
    if new_c >= m then
      let st = st |> State.inc_ly |> check_ly_lyc in
      if st.gpu_mem.lcd_regs.ly < screen_h then
        let st = if LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
        State.change_mode st @@ OAM_scan (new_c - m), ppu
      else
        let st = if LCD_Regs.mode1_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
        let st = State.request_VBlank st in
        State.change_mode st @@ VBlank (new_c - m), ppu
    else
      State.update_mode st @@ HBlank (new_c, m), ppu
  | GPUmode.VBlank c       ->
    let new_c = c + dots in
    if new_c >= line_duration then
      let st = st |> State.inc_ly |> check_ly_lyc in
      if st.gpu_mem.lcd_regs.ly < screen_h + 10 then
        State.update_mode st @@ VBlank (new_c - line_duration), ppu
      else
        let st = st |> reset_ly |> check_ly_lyc in
        let st = if LCD_Regs.mode2_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
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
      let st = if LCD_Regs.mode0_cond st.gpu_mem.lcd_regs then State.request_LCD st else st in
      State.change_mode st @@ HBlank (new_c, 204), ppu
    else
      State.update_mode st @@ Drawing_pixels (new_c, m), ppu

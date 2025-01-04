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

let ref framebuffer = Array.make_matrix 144 160 0

type pixel = { color: int; palette : int; sprite_prio : int; bg_prio : int; sprite_buffer : int list }

type t = { bg_fifo : pixel FIFO.t; obj_fifo : pixel FIFO.t }
let initial = { bg_fifo = FIFO.empty; obj_fifo = FIFO.empty }

let dot_of_mc mcycles speed =
  if speed then mcycles * 4 else mcycles * 2

(* TODO - actual oam scan *)
let scan_oam ppu st = ppu

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
      (* TODO: need to check if we need to go to vblank or oam scan *)
      { st with gpu_mem = { st.gpu_mem with mode = OAM_scan (new_c - m) } }, ppu
    else
      { st with gpu_mem = { st.gpu_mem with mode = HBlank (new_c, m) } }, ppu
  | GPUmode.VBlank c       ->
  let new_c = c + dots in
  if new_c >= 4560 then
    { st with gpu_mem = { st.gpu_mem with mode = OAM_scan (new_c - 4560) } }, ppu
  else
    { st with gpu_mem = { st.gpu_mem with mode = VBlank new_c } }, ppu
  | GPUmode.OAM_scan c    ->
    let new_c = c + dots in
    if new_c >= 80 then
      scan_oam ppu st, { st with gpu_mem = { st.gpu_mem with mode = Drawing_pixels (new_c - 80, new_c) } }
    else
      { st with gpu_mem = { st.gpu_mem with mode = OAM_scan new_c } }, ppu
  | GPUmode.Drawing_pixels (_, _) -> ppu, st
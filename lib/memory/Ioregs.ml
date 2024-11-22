(*
  $FF00   	      Joypad input
  $FF01   $FF02   Serial transfer
  $FF04   $FF07   Timer and divider
  $FF0F           Interrupts
  $FF10   $FF26   Audio
  $FF30   $FF3F   Wave pattern
  $FF40   $FF4B   LCD Control, Status, Position, Scrolling, and Palettes
  $FF4F           VRAM Bank Select
  $FF50           Set to non-zero to disable boot ROM
  $FF51   $FF55   VRAM DMA
  $FF68   $FF6B   BG / OBJ Palettes
  $FF70   	      WRAM Bank Select
*)


module Joypad = struct

end


module S = struct

  type t = {joypad : int; iflag : int}

  let in_range a n b = a <= n && n <= b
  let empty = { joypad = 0; iflag = 0 }

  let get r i =
    match i with
    (* Joypad *)
    | 0xFF00 -> r.joypad
    (* Serial data transfer - unimplemented *)
    | 0xFF01 | 0xFF02 -> 0
    (* Timer and divider *)
    | i when in_range 0xFF04 i 0xFF07 -> 0
    (* Interrupts *)
    | 0xFF0F -> r.iflag
    (* Audio - TODO *)
    | i when in_range 0xFF10 i 0xFF26 -> 0
    (* Wave pattern - TODO *)
    | i when in_range 0xFF30 i 0xFF3F -> 0
    (* LCD Control, Status, Position, Scrolling, and Palettes - TODO, maybe move *)
    | i when in_range 0xFF40 i 0xFF4B -> 0
    (* VRAM bank select  TODO: move to VRAM *)
    | 0xFF4F -> 0
    (* VRAM DMA          TODO: move to VRAM *)
    | i when in_range 0xFF51 i 0xFF55 -> 0
    (* BG / OBJ palettes TODO: move somewhere else *)
    | i when in_range 0xFF68 i 0xFF6B -> 0
    (* WRAM bank select  TODO: move to WRAM *)
    | 0xFF70 -> 0

  let set r i v =
    match i with
    (* Joypad *)
    | 0xFF00 -> r
    (* Serial data transfer - unimplemented *)
    | 0xFF01 | 0xFF02 -> r
    (* Timer and divider *)
    | i when in_range 0xFF04 i 0xFF07 -> r
    (* Interrupts *)
    | 0xFF0F -> r
    (* Audio - TODO *)
    | i when in_range 0xFF10 i 0xFF26 -> r
    (* Wave pattern -    TODO *)
    | i when in_range 0xFF30 i 0xFF3F -> r
    (* LCD Control, Status, Position, Scrolling, and Palettes - TODO, maybe move *)
    | i when in_range 0xFF40 i 0xFF4B -> r
    (* VRAM bank select  TODO: move to VRAM *)
    | 0xFF4F -> r
    (* VRAM DMA          TODO: move to VRAM *)
    | i when in_range 0xFF51 i 0xFF55 -> r
    (* BG / OBJ palettes TODO: move somewhere else *)
    | i when in_range 0xFF68 i 0xFF6B -> r
    (* WRAM bank select  TODO: move to WRAM *)
    | 0xFF70 -> r

  let in_range n = in_range 0xFF00 n 0xFF7F

end

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
  type t = int
  let empty = 0
  let get m _ = m
  let set _ _ v = v
  let in_range v = v = 0xFF00
end

(* TODO *)
module Serial = struct
  type t = int
  let empty = 0
  let get m _ = m
  let set _ _ v = v
  let in_range v = v = 0xFF01 || v = 0xFF02
end

(* TODO *)
module Timer = struct
  type t = { div : int; tima : int; tma : int; tac : int }
  let empty = { div = 0; tima = 0; tma = 0; tac = 0 }
  let reset_div m = { m with div = 0 }
  let get m i =
    match i with
    | 0xFF04 -> m.div
    | 0xFF05 -> m.tima
    | 0xFF06 -> m.tma
    | 0xFF07 -> m.tac
  let set m i v =
    match i with
    | 0xFF04 -> { m with div  = 0 }
    | 0xFF05 -> { m with tima = v }
    | 0xFF06 -> { m with tma  = v }
    | 0xFF07 -> { m with tac  = v }

  let reset_div m = { m with div = 0 }
  let in_range i = 0xFF04 <= i && i <= 0xFF07
end

(* TODO *)
module Interrupts = struct
  type t = int
  let empty = 0
  let get m _ = m
  let set _ _ v = v land 0b11111
  let in_range = (=) 0xFF0F
end

(* TODO *)
module Audio = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF10 <= i && i <= 0xFF26
end

(* TODO *)
module WavePattern = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF30 <= i && i <= 0xFF3F
end

(* TODO *)
module LCDControl = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF40 <= i && i <= 0xFF4B
end

(* VRAM bank select - in VRAM *)

(* VRAM DMA - in VRAM *)

(* TODO *)
module Palettes = struct
  type t = int
  let empty = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF68 <= i && i <= 0xFF6B
end

(* WRAM bank select - in WRAM *)

module IE = struct
  type t = int
  let empty = 0
  let get m _ = m
  let set m _ v = v land 0b11111
  let in_range i = i = 0xFFFF
end
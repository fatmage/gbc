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
  let initial = 0
  let get m _ = m
  let set _ _ v = v
  let in_range v = v = 0xFF00
end

(* TODO *)
module Serial = struct
  type t = int
  let initial = 0
  let get m _ = m
  let set _ _ v = v
  let in_range v = v = 0xFF01 || v = 0xFF02
end

module Timer = struct
  type t = { div : int; tima : int; tma : int; tac : int; speed : bool; div_c : int; tima_c : int }
  let initial = { div = 0; tima = 0; tma = 0; tac = 0; speed = false; div_c = 0; tima_c = 0 }
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
    | 0xFF05 ->
      {
        m with tima =
        match v with
        | 0b00 -> 256
        | 0b01 -> 4
        | 0b10 -> 16
        | 0b11 -> 64
      }
    | 0xFF06 -> { m with tma  = v }
    | 0xFF07 -> { m with tac  = v }

  let reset_div m = { m with div = 0 }

  let inc_div m = { m with div = m.div + 1 land 0xFF }

  let inc_tima m =
    let res = m.tima + 1 in
    if res > 0xFF then
      { m with tima = m.tma }, true
    else
      { m with tima = res }, false

  let tac_enabled m = m.tac land 0b100 = 0b100
  let tima_mcyc m = m.tima
  let mcyc_to_hz v double =
    if double then 1048576 / v else 2097152 / v
  let double_speed m = m.speed
  let switch_speed m = { m with speed = not m.speed }
  let run m cycles = { m with div_c = m.div_c + cycles; tima_c = m.tima_c + cycles }
  let in_range i = 0xFF04 <= i && i <= 0xFF07
end

module Interrupts = struct
  type t = int
  let initial = 0
  let get m _ = m
  let set _ _ v = v land 0b11111
  let in_range = (=) 0xFF0F
end

(* TODO *)
module Audio = struct
  type t = int
  let initial = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF10 <= i && i <= 0xFF26
end

(* TODO *)
module WavePattern = struct
  type t = int
  let initial = 0
  let get _ _ = 0
  let set _ _ _ = 0
  let in_range i = 0xFF30 <= i && i <= 0xFF3F
end

(* LCD Control - in graphics *)

(* VRAM bank select - in VRAM *)

(* VRAM DMA - in VRAM *)

(* Palettes - in graphics *)

(* WRAM bank select - in WRAM *)

module IE = struct
  type t = int
  let initial = 0
  let get m _ = m
  let set m _ v = v land 0b11111
  let in_range i = i = 0xFFFF
end

module CGB_Regs = struct
  type t = { hdma1 : int; hdma2 : int; hdma3 : int; hdma4 : int; hdma5 : int;
  key1 : int}

  let initial = { hdma1 = 0; hdma2 = 0; hdma3 = 0; hdma4 = 0; hdma5 = 0; key1 = 0 }

  let get m _ = 0
  let set m _ v = m
  let in_range _ = false
end
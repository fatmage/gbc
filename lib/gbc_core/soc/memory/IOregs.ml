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
  let initial = 0xCF
  let get m _ = m
  let set m _ v = m land 0x0F lor (v land 0xF0)
  let set_input m v = v
  let in_range v = v = 0xFF00
end

(* TODO *)
module Serial = struct
  type t = { sb : int; sc : int }
  let initial = { sb = 0; sc = 0x7F }
  let get m i =
    match i with
    | 0xFF01 -> m.sb
    | 0xFF02 -> m.sc
  let set m i v = m
  let in_range v = v = 0xFF01 || v = 0xFF02
end

module Timer = struct
  type t = { div : int; tima : int; tma : int; tac : int; speed : bool; div_c : int; tima_c : int; key1 : int }
  let initial = { div = 0; tima = 0; tma = 0; tac = 0xF8; speed = false; div_c = 0; tima_c = 0; key1 = 0x7E }
  let reset_div m = { m with div = 0 }

  let get m i =
    match i with
    | 0xFF04 -> m.div
    | 0xFF05 ->
      begin match m.tima with
      | 256 -> 0b00
      | 4   -> 0b01
      | 16  -> 0b10
      | 64  -> 0b11
      end
    | 0xFF06 -> m.tma
    | 0xFF07 -> m.tac
    | 0xFF4D -> m.key1

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
    | 0xFF4D -> { m with key1 = (m.key1 land 0x80) lor (v land 0x7F) }

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
  let switch_speed m = { m with speed = not m.speed; key1 = m.key1 lxor 0x80 }
  let run_div m cycles =
    let rec aux m =
      function
      | 0 -> m
      | n -> aux (inc_div m) (n - 1)
    in
    let div_c = m.div_c + cycles in
    let ticks = div_c / 64 in
    let m = aux m ticks in
    { m with div_c = div_c mod 64 }

  let run_tima m cycles =
    let rec aux (m,i) =
      function
      | 0 -> m,i
      | n -> aux (inc_tima m) (n - 1)
    in
    match tac_enabled m with
    | false -> m, false
    | true  ->
      let tima_c = m.tima_c + cycles in
      let cpt = tima_mcyc m in
      let ticks = tima_c / cpt in
      let m, interrupted = aux (m, false) ticks in
      { m with tima_c = tima_c mod cpt }, interrupted

  let get_speed m = m.speed

  let tmul m = if m.speed then 1. /. 8388608. else 1. /. 4194304.
  let in_range i = (0xFF04 <= i && i <= 0xFF07) || i = 0xFF4D
end

module Interrupts = struct
  type t = int
  let initial = 0xE1
  let get m _ = m
  let set _ _ v = v land 0b11111
  let request_joypad m = m lor 0b10000
  let request_serial m = m lor 0b01000
  let request_timer m  = m lor 0b00100
  let request_LCD m    = m lor 0b00010
  let request_VBlank m = m lor 0b00001
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

  let enabled_joypad m = m lor 0b10000 > 0
  let enabled_serial m = m lor 0b01000 > 0
  let enabled_timer m  = m lor 0b00100 > 0
  let enabled_LCD m    = m lor 0b00010 > 0
  let enabled_VBlank m = m lor 0b00001 > 0

  let in_range = (=) 0xFFFF
end



module CGB_Regs = struct
  type t = { key0 : int; key1 : int; opri : int; rp : int }

  let initial = { key0 = 0; key1 = 0; opri = 0; rp = 0 }

  let get m _ = 0
  let set m _ v = m
  let in_range _ = false
end
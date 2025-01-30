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
  type t = { chosen : int; buttons : int; dpad : int }
  let initial = { chosen = 0b110000; buttons = 0xF; dpad = 0xF }
  let get m _ =
    match m.chosen with
    | 0b110000 -> 0xCF
    | 0b010000 -> 0xC0 lor m.buttons
    | 0b100000 -> 0xC0 lor m.dpad
    | 0b000000 -> 0xC0 lor m.buttons

  let set m _ v =
    (* Utils.print_hex "Setujemy joypad" v; *)
    (* let _ = read_line () in *)
    { m with chosen = v land 0x30 }

  let set_input m b d =
    (* print_endline "setujemy joypad";
    Utils.print_hex "Chosen" m.chosen;
    Utils.print_hex "B" b;
    Utils.print_hex "D" d; *)
    (* Utils.print_hex "Buttons" m.buttons; *)
    (* Utils.print_hex "Dpad" m.dpad; *)

    match b, d with
    | buttons, dpad when b = m.buttons && d = m.dpad ->
      (* print_endline "no change"; *)
      m
    | buttons, _ when d = m.dpad ->
      { m with buttons }
    | _, dpad when b = m.buttons ->
      { m with dpad }
    | buttons, dpad ->
      { m with buttons; dpad }

  let get_input m = m.buttons, m.dpad

  let joypad_diff m b d = not (b = m.buttons && d = m.dpad)
  let in_range v = v = 0xFF00
end

(* TODO *)
module Serial = struct
  type t = { sb : int; sc : int; rp : int }
  let initial = { sb = 0; sc = 0x7F; rp = 0}
  let get m i =
    match i with
    | 0xFF01 -> m.sb
    | 0xFF02 -> m.sc
    | _ -> assert false
  let set m _ _ = m
  let in_range v = v = 0xFF01 || v = 0xFF02 || v = 0xFF56
end

module Timer = struct
  type t = { div : int; tima : int; tma : int; tac : int; speed : bool; div_c : int; tima_c : int; key1 : int }
  let initial = { div = 0; tima = 0; tma = 0; tac = 0xF8; speed = false; div_c = 0; tima_c = 0; key1 = 0x7E }
  (* let reset_div m = { m with div = 0 } *)

  let get m i =
    match i with
    | 0xFF04 -> m.div
    | 0xFF05 -> m.tima
      (* begin match m.tima with
      | 256 -> 0b00
      | 4   -> 0b01
      | 16  -> 0b10
      | 64  -> 0b11
      | _   -> assert false
      end *)
    | 0xFF06 -> m.tma
    | 0xFF07 -> m.tac
    | 0xFF4D -> m.key1
    | _ -> assert false

  let set m i v =
    match i with
    | 0xFF04 -> { m with div  = 0 }
    | 0xFF05 -> { m with tima = v }
    | 0xFF06 -> { m with tma  = v }
    | 0xFF07 -> { m with tac  = v land 0b111 }
    | 0xFF4D -> { m with key1 = (m.key1 land 0xFE) lor v }
    | _    -> assert false

  let reset_div m = { m with div = 0 }

  let inc_div m = { m with div = (m.div + 1) land 0xFF }

  let inc_tima m =
    let res = m.tima + 1 in
    if res > 0xFF then
      { m with tima = m.tma }, true
    else
      { m with tima = res }, false

  let tac_enabled m = m.tac land 0b100 > 0
  let tima_mcyc m =
    match m.tac land 0b11 with
    | 0b00 -> 256
    | 0b01 -> 4
    | 0b10 -> 16
    | 0b11 -> 64
  let mcyc_to_hz v double =
    if double then 1048576 / v else 2097152 / v
  let switch_speed m = { m with speed = not m.speed; key1 = m.key1 lxor 0x81 }
  let switch_requested m = m.key1 land 0x01 > 0
  let run_div m cycles =
    let rec aux m =
      function
      | 0 -> m
      | n -> aux (inc_div m) (n - 1)
    in
    let div_c = m.div_c + cycles in
    let ticks = div_c / 256 in
    let m = aux m ticks in
    { m with div_c = div_c mod 256 }

  let run_tima m cycles =
    let rec aux (m,i) =
      function
      | 0 -> m,i
      | n ->
        let m,i' = inc_tima m in
        aux (m, i || i') (n - 1)
    in
    match tac_enabled m with
    | false -> m, false
    | true  ->
      let tima_c = m.tima_c + cycles in
      let cpt = tima_mcyc m in
      let ticks = tima_c / cpt in
      (* Utils.print_hex "Tima c" m.tima_c;
      Utils.print_hex "cycles" cycles;
      Utils.print_dec "CPT" cpt;
      Utils.print_dec "Ticks" ticks;
      Utils.print_dec "new tima c" @@ tima_c mod cpt; *)
      let m, interrupted = aux (m, false) ticks in
      { m with tima_c = tima_c mod cpt }, interrupted

  let get_speed m = m.speed

  let tmul m = if m.speed then 4. /. 8388608. else 4. /. 4194304.
  let in_range i = (0xFF04 <= i && i <= 0xFF07) || i = 0xFF4D
end

module Interrupts = struct
  type t = int
  let initial = 0xE1
  let get m _ = m lor 0xE0
  let set _ _ v = v land 0x1F
  let request_joypad m = m lor 0b10000
  let request_serial m = m lor 0b01000
  let request_timer m  = m lor 0b00100
  let request_LCD m    = m lor 0b00010
  let request_VBlank m = m lor 0b00001
  let handled_joypad m = m land 0b11101111
  let handled_serial m = m land 0b11110111
  let handled_timer m  = m land 0b11111011
  let handled_LCD m    = m land 0b11111101
  let handled_VBlank m = m land 0b11111110
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
  let set _ _ v = v

  let enabled_joypad m = m lor 0b10000 > 0
  let enabled_serial m = m lor 0b01000 > 0
  let enabled_timer m  = m lor 0b00100 > 0
  let enabled_LCD m    = m lor 0b00010 > 0
  let enabled_VBlank m = m lor 0b00001 > 0

  let in_range = (=) 0xFFFF
end

(* module CGB_Regs = struct
  type t = { key0 : int; key1 : int; opri : int; rp : int }

  let initial = { key0 = 0; key1 = 0; opri = 0; rp = 0 }

  let get m _ = 0
  let set m _ v = m
  let in_range _ = false
end *)


type t = { start : bool; select: bool; b : bool; a : bool; down : bool; up : bool; left : bool; right : bool }

let dpad_to_int joypad =
  let down  = if joypad.down  then 0 else 0b1000 in
  let up    = if joypad.up    then 0 else 0b0100 in
  let left  = if joypad.left  then 0 else 0b0010 in
  let right = if joypad.right then 0 else 0b0001 in
  down lor up lor left lor right

let bttn_to_int joypad =
  let start  = if joypad.start  then 0 else 0b1000 in
  let select = if joypad.select then 0 else 0b0100 in
  let b      = if joypad.b      then 0 else 0b0010 in
  let a      = if joypad.a      then 0 else 0b0001 in
  start lor select lor b lor a

let set_joypad joypad reg =
  let high_nibble = reg land 0xF0 in
  match reg land 0x30 with
  | 0b10 -> high_nibble lor (bttn_to_int joypad)
  | 0b01 -> high_nibble lor (dpad_to_int joypad)
  | _    -> high_nibble lor 0x0F
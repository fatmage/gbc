open Tsdl

type t = { start : bool; select: bool; b : bool; a : bool; down : bool; up : bool; left : bool; right : bool }

type button = Start | Select | B | A | Down | Up | Left | Right
let input = ref { start = true; select = true; b = true; a = true; down = true; up = true; left = true; right = true }
let switch_mode = ref false

let dpad_to_int () =
  let down  = if !input.down  then 0 else 0b1000 in
  let up    = if !input.up    then 0 else 0b0100 in
  let left  = if !input.left  then 0 else 0b0010 in
  let right = if !input.right then 0 else 0b0001 in
  down lor up lor left lor right

let bttn_to_int () =
  let start  = if !input.start  then 0 else 0b1000 in
  let select = if !input.select then 0 else 0b0100 in
  let b      = if !input.b      then 0 else 0b0010 in
  let a      = if !input.a      then 0 else 0b0001 in
  start lor select lor b lor a

let press_button =
  function
  | Start  -> input := { !input with start = false }
  | Select -> input := { !input with select = false }
  | B      -> input := { !input with b = false }
  | A      -> input := { !input with a = false }
  | Down   -> input := { !input with down = false }
  | Up     -> input := { !input with up = false }
  | Left   -> input := { !input with left = false }
  | Right  -> input := { !input with right = false }

let release_button =
  function
  | Start  -> input := { !input with start = true }
  | Select -> input := { !input with select = true }
  | B      -> input := { !input with b = true }
  | A      -> input := { !input with a = true }
  | Down   -> input := { !input with down = true }
  | Up     -> input := { !input with up = true }
  | Left   -> input := { !input with left = true }
  | Right  -> input := { !input with right = true }

let handle_events () =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
      | `S -> press_button Down
      | `W -> press_button Up
      | `A -> press_button Left
      | `D -> press_button Right
      | `K -> press_button B
      | `L -> press_button A
      | `Return -> press_button Start
      | `Lshift -> press_button Select
      | `Escape -> exit 0
      | `Space  -> switch_mode := true
      | _ -> ()
      end
    | `Key_up ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
      | `S -> release_button Down
      | `W -> release_button Up
      | `A -> release_button Left
      | `D -> release_button Right
      | `K -> release_button B
      | `L -> release_button A
      | `Return -> release_button Start
      | `Lshift -> release_button Select
      | _ -> ()
      end
    | `Quit -> exit 0
    | _     -> ()
  done

let set_joypad reg =
  let high_nibble = reg land 0xF0 in
  match reg land 0x30 with
  | 0b10 -> high_nibble lor (bttn_to_int ())
  | 0b01 -> high_nibble lor (dpad_to_int ())
  | _    -> high_nibble lor 0x0F

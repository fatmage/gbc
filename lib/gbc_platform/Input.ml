open Tsdl

type gbc_control = { start : bool; select: bool; b : bool; a : bool; down : bool; up : bool; left : bool; right : bool }

type button = Start | Select | B | A | Down | Up | Left | Right
let gbc_input = ref { start = true; select = true; b = true; a = true; down = true; up = true; left = true; right = true }

type dbg_control = { back : bool; forward : bool; modifier : bool }
let dbg_input = ref { back = false; forward = false; modifier = false }
let switch_mode = ref false

let dpad_to_int () =
  let down  = if !gbc_input.down  then 0 else 0b1000 in
  let up    = if !gbc_input.up    then 0 else 0b0100 in
  let left  = if !gbc_input.left  then 0 else 0b0010 in
  let right = if !gbc_input.right then 0 else 0b0001 in
  down lor up lor left lor right

let bttn_to_int () =
  let start  = if !gbc_input.start  then 0 else 0b1000 in
  let select = if !gbc_input.select then 0 else 0b0100 in
  let b      = if !gbc_input.b      then 0 else 0b0010 in
  let a      = if !gbc_input.a      then 0 else 0b0001 in
  start lor select lor b lor a

let press_button =
  function
  | Start  -> gbc_input := { !gbc_input with start = false }
  | Select -> gbc_input := { !gbc_input with select = false }
  | B      -> gbc_input := { !gbc_input with b = false }
  | A      -> gbc_input := { !gbc_input with a = false }
  | Down   -> gbc_input := { !gbc_input with down = false }
  | Up     -> gbc_input := { !gbc_input with up = false }
  | Left   -> gbc_input := { !gbc_input with left = false }
  | Right  -> gbc_input := { !gbc_input with right = false }

let release_button =
  function
  | Start  -> gbc_input := { !gbc_input with start = true }
  | Select -> gbc_input := { !gbc_input with select = true }
  | B      -> gbc_input := { !gbc_input with b = true }
  | A      -> gbc_input := { !gbc_input with a = true }
  | Down   -> gbc_input := { !gbc_input with down = true }
  | Up     -> gbc_input := { !gbc_input with up = true }
  | Left   -> gbc_input := { !gbc_input with left = true }
  | Right  -> gbc_input := { !gbc_input with right = true }

let switch_button =
  function
  | Start  -> gbc_input := { !gbc_input with start = not !gbc_input.start }
  | Select -> gbc_input := { !gbc_input with select = not !gbc_input.select }
  | B      -> gbc_input := { !gbc_input with b = not !gbc_input.b }
  | A      -> gbc_input := { !gbc_input with a = not !gbc_input.a }
  | Down   -> gbc_input := { !gbc_input with down = not !gbc_input.down }
  | Up     -> gbc_input := { !gbc_input with up = not !gbc_input.up }
  | Left   -> gbc_input := { !gbc_input with left = not !gbc_input.left }
  | Right  -> gbc_input := { !gbc_input with right = not !gbc_input.right }

let handle_emulator_events () =
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

let handle_debugger_events () =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
      | `Escape -> exit 0
      | `Space  -> switch_mode := true
      | `Lctrl  -> dbg_input := { !dbg_input with modifier = true }
      | `Left   -> dbg_input := { !dbg_input with back = true }
      | `Right  -> dbg_input := { !dbg_input with forward = true }
      | _ -> ()
      end
    | `Key_up ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
      | `S -> switch_button Down
      | `W -> switch_button Up
      | `A -> switch_button Left
      | `D -> switch_button Right
      | `K -> switch_button B
      | `L -> switch_button A
      | `Return -> switch_button Start
      | `Lshift -> switch_button Select
      | `Lctrl -> dbg_input := { !dbg_input with modifier = false }
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

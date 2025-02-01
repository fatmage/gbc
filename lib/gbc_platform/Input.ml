open Tsdl

type gbc_control = {
  start : bool;
  select : bool;
  b : bool;
  a : bool;
  down : bool;
  up : bool;
  left : bool;
  right : bool;
}

type button = Start | Select | B | A | Down | Up | Left | Right

let gbc_input =
  ref
    {
      start = false;
      select = false;
      b = false;
      a = false;
      down = false;
      up = false;
      left = false;
      right = false;
    }

type dbg_control = { back : bool; forward : bool; ctrl : bool; alt : bool }

let dbg_input = ref { back = false; forward = false; ctrl = false; alt = false }
let switch_mode = ref false

let dpad_to_int () =
  let down = if !gbc_input.down then 0 else 0b1000 in
  let up = if !gbc_input.up then 0 else 0b0100 in
  let left = if !gbc_input.left then 0 else 0b0010 in
  let right = if !gbc_input.right then 0 else 0b0001 in
  down lor up lor left lor right

let bttn_to_int () =
  let start = if !gbc_input.start then 0 else 0b1000 in
  let select = if !gbc_input.select then 0 else 0b0100 in
  let b = if !gbc_input.b then 0 else 0b0010 in
  let a = if !gbc_input.a then 0 else 0b0001 in
  start lor select lor b lor a

let press_button = function
  | Start -> gbc_input := { !gbc_input with start = true }
  | Select -> gbc_input := { !gbc_input with select = true }
  | B -> gbc_input := { !gbc_input with b = true }
  | A -> gbc_input := { !gbc_input with a = true }
  | Down -> gbc_input := { !gbc_input with down = true }
  | Up -> gbc_input := { !gbc_input with up = true }
  | Left -> gbc_input := { !gbc_input with left = true }
  | Right -> gbc_input := { !gbc_input with right = true }

let release_button = function
  | Start -> gbc_input := { !gbc_input with start = false }
  | Select -> gbc_input := { !gbc_input with select = false }
  | B -> gbc_input := { !gbc_input with b = false }
  | A -> gbc_input := { !gbc_input with a = false }
  | Down -> gbc_input := { !gbc_input with down = false }
  | Up -> gbc_input := { !gbc_input with up = false }
  | Left -> gbc_input := { !gbc_input with left = false }
  | Right -> gbc_input := { !gbc_input with right = false }

let switch_button = function
  | Start ->
      gbc_input := { !gbc_input with start = not !gbc_input.start };
      if !gbc_input.start then print_endline "Switch button pressed."
      else print_endline "Switch button released."
  | Select ->
      gbc_input := { !gbc_input with select = not !gbc_input.select };
      if !gbc_input.select then print_endline "Select button pressed."
      else print_endline "Select button released."
  | B ->
      gbc_input := { !gbc_input with b = not !gbc_input.b };
      if !gbc_input.b then print_endline "B button pressed."
      else print_endline "B button released."
  | A ->
      gbc_input := { !gbc_input with a = not !gbc_input.a };
      if !gbc_input.a then print_endline "A button pressed."
      else print_endline "A button released."
  | Down ->
      gbc_input := { !gbc_input with down = not !gbc_input.down };
      if !gbc_input.down then print_endline "Down button pressed."
      else print_endline "Down button released."
  | Up ->
      gbc_input := { !gbc_input with up = not !gbc_input.up };
      if !gbc_input.up then print_endline "Up button pressed."
      else print_endline "Up button released."
  | Left ->
      gbc_input := { !gbc_input with left = not !gbc_input.left };
      if !gbc_input.left then print_endline "Left button pressed."
      else print_endline "Left button released."
  | Right ->
      gbc_input := { !gbc_input with right = not !gbc_input.right };
      if !gbc_input.right then print_endline "Right button pressed."
      else print_endline "Right button released."

let handle_emulator_events () =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down -> (
        let scancode = Sdl.Event.(get event keyboard_scancode) in
        match Sdl.Scancode.enum scancode with
        | `S -> press_button Down
        | `W -> press_button Up
        | `A -> press_button Left
        | `D -> press_button Right
        | `L -> press_button B
        | `Semicolon -> press_button A
        | `Return -> press_button Start
        | `Lshift -> press_button Select
        | `Escape -> exit 0
        | `Space -> switch_mode := true
        | _ -> ())
    | `Key_up -> (
        let scancode = Sdl.Event.(get event keyboard_scancode) in
        match Sdl.Scancode.enum scancode with
        | `S -> release_button Down
        | `W -> release_button Up
        | `A -> release_button Left
        | `D -> release_button Right
        | `L -> release_button B
        | `Semicolon -> release_button A
        | `Return -> release_button Start
        | `Lshift -> release_button Select
        | _ -> ())
    | `Quit -> exit 0
    | _ -> ()
  done

let handle_debugger_events () =
  let event = Sdl.Event.create () in
  while Sdl.poll_event (Some event) do
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down -> (
        let scancode = Sdl.Event.(get event keyboard_scancode) in
        match Sdl.Scancode.enum scancode with
        | `Escape -> exit 0
        | `Space -> switch_mode := true
        | `Lctrl -> dbg_input := { !dbg_input with ctrl = true }
        | `Lalt -> dbg_input := { !dbg_input with alt = true }
        | `Left -> dbg_input := { !dbg_input with back = true }
        | `Right -> dbg_input := { !dbg_input with forward = true }
        | _ -> ())
    | `Key_up -> (
        let scancode = Sdl.Event.(get event keyboard_scancode) in
        match Sdl.Scancode.enum scancode with
        | `S -> switch_button Down
        | `W -> switch_button Up
        | `A -> switch_button Left
        | `D -> switch_button Right
        | `L -> switch_button B
        | `Semicolon -> switch_button A
        | `Return -> switch_button Start
        | `Lshift -> switch_button Select
        | `Lctrl -> dbg_input := { !dbg_input with ctrl = false }
        | `Lalt -> dbg_input := { !dbg_input with alt = false }
        | _ -> ())
    | `Quit -> exit 0
    | _ -> ()
  done

let set_joypad () = (bttn_to_int (), dpad_to_int ())

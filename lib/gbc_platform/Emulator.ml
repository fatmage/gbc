
let frame_time = 1. /. 60.

module type S = sig
  type state
  module History : History.S

  val emulator_loop : state -> History.t -> float -> float -> Tsdl.Sdl.texture -> Tsdl.Sdl.renderer -> unit
end


module Make (GBC : Gbc_core.CPU.S) : (S with type state = GBC.State.t) = struct
  type state = GBC.State.t
  module History = History.Make(GBC.State)

  let rec emulator_loop  (st : state) (history : History.t) prev_time delta_time texture renderer =
    Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
    Input.handle_emulator_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch display mode to debugger mode *)
      debugger_loop st history texture renderer
    else
      let curr_time = Sys.time () in
      let delta_time = (curr_time -. prev_time) +. delta_time in
      if (delta_time > frame_time) then
        let st, time = GBC.cpu_step st in
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        (* Final state in this step *)
        let history = History.add_state history st in
        emulator_loop st history curr_time delta_time texture renderer
      else
        emulator_loop st history curr_time delta_time texture renderer
  and debugger_loop (st : state) (history : History.t) texture renderer =
    Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
    Input.handle_debugger_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch display mode to emulator mode *)
      let time = Sys.time () in
      emulator_loop st history time 0. texture renderer
    else
      (* TODO: implement debugger loop *)
      match !Input.dbg_input with
      | { back = true; modifier;_ } ->
        let move_back hs =
          if modifier then History.move_back hs else History.move_back_n hs 100000
        in
        let history = move_back history in
        let st = History.get history in
        debugger_loop st history texture renderer
      | { forward = true; modifier; _ } ->
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        let st, _ = GBC.cpu_step st in
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        let history = History.add_state history st in
        debugger_loop st history texture renderer
      | _ -> debugger_loop st history texture renderer





end

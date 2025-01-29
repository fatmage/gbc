
let frame_time = 1. /. 60.

module type S = sig
  type state
  module History : History.S

  val emulator_loop : state -> History.t -> float -> float -> float -> Tsdl.Sdl.texture -> Tsdl.Sdl.renderer -> unit
  val debugger_loop : state -> History.t -> Tsdl.Sdl.texture -> Tsdl.Sdl.renderer -> unit
end


module Make (GBC : Gbc_core.CPU.S) : (S with type state = GBC.State.t) = struct
  type state = GBC.State.t
  module History = History.Make(GBC)

  let rec emulator_loop  (st : state) (history : History.t)
    prev_time delta_time time_left texture renderer =
    Input.handle_emulator_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch to debugger mode *)
      debugger_loop st history texture renderer
    else
      let curr_time = Sys.time () in
      let delta_time = (curr_time -. prev_time) +. delta_time in
      if (delta_time > time_left) then
        let delta_time = delta_time -. time_left in
        let st, cpu_time, frame_end = GBC.cpu_step st in
        let _ = if frame_end then
          Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer
        else
          ()
        in
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        (* Final state in this step *)
        let history = History.add_state history st frame_end in
        emulator_loop st history curr_time delta_time cpu_time texture renderer
      else
        emulator_loop st history curr_time delta_time time_left texture renderer
  and debugger_loop (st : state) (history : History.t) texture renderer =
    Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
    Input.handle_debugger_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch to emulator mode *)
      let time = Sys.time () in
      emulator_loop st history time 0. 0. texture renderer
    else
      match !Input.dbg_input with
      | { back = true; ctrl; alt;_ } ->
        Input.dbg_input := { !Input.dbg_input with back= false };
        let move_back hs =
          if ctrl then
            let _ = print_endline "Moving back one frame" in
            History.move_back_frame hs
          else
            if alt then
              let _ = print_endline "Moving back one second" in
              History.move_back_second hs
            else
              let _ = print_endline "Moving back one state" in
              History.move_back hs
        in
        let st, history = if History.is_empty history then st, history else move_back history in
        Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
        GBC.print_registers st;
        GBC.print_interrupts st;
        debugger_loop st history texture renderer
      | { forward = true;  _ } ->
        Input.dbg_input := { !Input.dbg_input with forward = false };
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        let st, _, frame_end = GBC.cpu_step st in
        let joypad = GBC.State.get_joypad st in
        let st = GBC.State.set_joypad st @@ Input.set_joypad joypad in
        (* Final state in this step *)
        let history = History.add_state history st frame_end in
        debugger_loop st history texture renderer
      | _ -> debugger_loop st history texture renderer





end

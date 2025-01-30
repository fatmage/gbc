
let frame_time = 1. /. 60.

module type S = sig
  type state
  module History : History.S

  val emulator_loop : state -> History.t -> float -> float -> float -> Tsdl.Sdl.texture -> Tsdl.Sdl.renderer -> unit
  val debugger_loop : state -> History.t -> Tsdl.Sdl.texture -> Tsdl.Sdl.renderer -> unit
end


module Make (GBC : Gbc_core.CGB.S) : (S with type state = GBC.State.t) = struct
  type state = GBC.State.t
  module History = History.Make(GBC)

  let rec emulator_loop  (st : state) (history : History.t)
    prev_time delta_time time_left texture renderer =
    Input.handle_emulator_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch to debugger mode *)
      print_endline "Entering debugger mode.";
      debugger_loop st history texture renderer
    else
      (* let curr_time = Sys.time () in *)
      (* let delta_time = (curr_time -. prev_time) +. delta_time in *)
      if (* (delta_time > time_left) *) true then
        (* let delta_time = delta_time -. time_left in *)
        let st, cpu_time, frame_end = GBC.cpu_step st in
        let _ = if frame_end then
          Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer
        else
          ()
        in
        let buttons, dpad = Input.set_joypad () in
        let st = GBC.State.set_joypad st buttons dpad in
        (* Final state in this step *)
        let history = History.add_state history st frame_end in
        emulator_loop st history (* curr_time delta_time cpu_time *) 0. 0. 0. texture renderer
      else
        emulator_loop st history (* curr_time delta_time time_left *) 0. 0. 0. texture renderer
  and debugger_loop (st : state) (history : History.t) texture renderer =
    Input.handle_debugger_events ();
    if !Input.switch_mode then
      let _ = Input.switch_mode := false in
      (* switch to emulator mode *)
      let time = Sys.time () in
      print_endline "Entering emulator mode";
      emulator_loop st history time 0. 0. texture renderer
    else
      let rec move_forward st history =
        function
        | false ->
          let buttons, dpad = Input.set_joypad () in
          let st = GBC.State.set_joypad st buttons dpad  in
          let st, _, frame_end = GBC.cpu_step st in
          let buttons, dpad = Input.set_joypad () in
          let st = GBC.State.set_joypad st buttons dpad  in
          let history = History.add_state history st frame_end in
          print_endline "Moving forward one state.";
          st, history
        | true ->
          let buttons, dpad = Input.set_joypad () in
          let st = GBC.State.set_joypad st buttons dpad in
          let st, _, frame_end = GBC.cpu_step st in
          let history = History.add_state history st frame_end in
          if frame_end then
            let buttons, dpad = Input.set_joypad () in
            let st = GBC.State.set_joypad st buttons dpad in
            let history = History.add_state history st frame_end in
            print_endline "Moving forward one frame.";
            st, history
          else
            move_forward st history true
      in
      match !Input.dbg_input with
      | { back = true; ctrl; alt;_ } ->
        Input.dbg_input := { !Input.dbg_input with back = false };
        let move_back hs =
          match ctrl, alt with
          | false, false ->
            let _ = print_endline "Moving back one frame." in
            History.move_back_frame hs
          | true, false ->
            let _ = print_endline "Moving back one state." in
            History.move_back hs
          | _ , true ->
            let _ = print_endline "Moving back one second." in
            History.move_back_second hs
        in
        let st, history = if History.is_empty history then st, history else move_back history in
        Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
        GBC.print_instructions st;
        GBC.print_registers st;
        GBC.print_interrupts st;
        debugger_loop st history texture renderer
      | { forward = true; ctrl; _ } ->
        Input.dbg_input := { !Input.dbg_input with forward = false };
        let st, history = move_forward st history (not ctrl) in
        Graphics.render_framebuffer texture renderer GBC.PPU.framebuffer;
        GBC.print_registers st;
        GBC.print_interrupts st;
        debugger_loop st history texture renderer
      | _ -> debugger_loop st history texture renderer





end

let usage_exit () =
  prerr_endline "Usage: gbc <file_path>";
  exit 1

let () =
  let argv = Sys.argv in
  let _ = if Array.length argv < 2 then usage_exit () in
  let rom_ch = open_in_bin argv.(1) in
  let rom = really_input_string rom_ch @@ in_channel_length rom_ch in
  let module Core = (val Gbc_core.GBC.gbc_module rom) in
  let gbc = Core.init_gb rom in
  let module GameBoyCaml = Gbc_platform.Emulator.Make (Core) in
  let renderer = Gbc_platform.Graphics.init_graphics () in
  let texture = Gbc_platform.Graphics.create_texture renderer in
  Gbc_platform.Graphics.render_framebuffer texture renderer Core.PPU.framebuffer;
  (* GameBoyCaml.emulator_loop gbc GameBoyCaml.History.empty (Sys.time ()) 0. false texture renderer *)
  GameBoyCaml.debugger_loop gbc GameBoyCaml.History.empty texture renderer

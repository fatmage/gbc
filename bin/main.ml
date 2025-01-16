let usage_exit () =
  prerr_endline "Usage: gbc <file_path>";
  exit 1


let () =
  let argv = Sys.argv in
  let _ = if Array.length argv < 2 then usage_exit () in
  let rom_ch = open_in_bin argv.(1) in
  let rom = Bytes.create (in_channel_length rom_ch) in
  really_input rom_ch rom 0 (in_channel_length rom_ch);
  let module Implementation = (val Gbc_core.GBC.gbc_module rom) in
  let gbc = Implementation.init_gb rom in
  let module GameBoyCaml = Gbc_platform.Emulator.Make(Implementation) in
  let renderer = Gbc_platform.Graphics.init_graphics () in
  let texture = Gbc_platform.Graphics.create_texture renderer in
  GameBoyCaml.emulator_loop gbc GameBoyCaml.History.empty (Sys.time ()) 0. 0. texture renderer

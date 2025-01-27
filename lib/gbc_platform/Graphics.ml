open Tsdl

let gbc_w = 160
let gbc_h = 144
let scale = 3.
let scaled_gbc_w = Float.(of_int gbc_w *. scale |> to_int)
let scaled_gbc_h = Float.(of_int gbc_h *. scale |> to_int)
let sec_per_frame = 1. /. 60.

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let init_graphics () =
  Sdl.init Sdl.Init.(video + events) |> or_exit;
  let window =
    Sdl.create_window ~w:scaled_gbc_w ~h:scaled_gbc_h "Gameboy Caml" Sdl.Window.windowed |> or_exit
  in
  Sdl.create_renderer window ~index:(-1) |> or_exit

let create_texture renderer =
  Sdl.create_texture renderer Sdl.Pixel.format_bgr555
  Sdl.Texture.access_streaming ~w:gbc_w ~h:gbc_h
  |> or_exit

let render_framebuffer texture renderer framebuffer =
  let copy_fb_to_pixels framebuffer pixels =
    for y = 0 to gbc_h - 1 do
      for x = 0 to gbc_w -1 do
        let index = (y * gbc_w) + x in
        (* pixels.{index} <- 0xED7E lsr 1; *)
        pixels.{index} <- framebuffer.(y).(x);
        if y = 5 && x = 122 then
          ()
          (* let _ = print_endline "PRAWDZIWY KOLOR" in *)
          (* let _ = print_int @@ framebuffer.(y).(x) lor 0x0080 in *)
          (* let _ = read_line () in () *)

      done
    done
  in
  Sdl.lock_texture texture None Bigarray.int16_unsigned |> or_exit
  |> (fun (pixels, _) -> pixels)
  |> copy_fb_to_pixels framebuffer;
  Sdl.unlock_texture texture;
  Sdl.render_copy renderer texture |> or_exit;
  Sdl.render_present renderer

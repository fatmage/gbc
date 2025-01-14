open Tsdl

let gb_w = 160
let gb_h = 144
let scale = 2.
let scaled_gb_w = Float.(of_int gb_w *. scale |> to_int)
let scaled_gb_h = Float.(of_int gb_h *. scale |> to_int)
let sec_per_frame = 1. /. 60.

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

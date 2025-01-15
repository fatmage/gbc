
let frame_time = 1. /. 60.


(* let rec interpreter_loop (type a) (module GBC : Gbc_core.CPU.S with type state = a ) (st : a) texture renderer =
  let cnt = ref 0 in
  let start_time = ref (Unix.gettimeofday ()) in
  let st, time = GBC.cpu_step st in  *)

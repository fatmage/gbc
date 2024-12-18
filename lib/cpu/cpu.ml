open Fetch_decode
open State

let first_set_bit =
  let rec aux n m k =
    match n land m with
    | 0 -> aux n (m * 2) (k + 1)
    | _ -> k
  in
  function
  | 0 -> 0
  | n -> aux n 1 1

let fetch_decode_execute st =
  let st, instr =
    match st.ime with
    | Enabled  ->
      let addr =
        match Ioregs.IE.get st.ie 0xFFFF land Ioregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
        | 1 -> 0x40 (* VBlank *)
        | 2 -> 0x48 (* LCD *)
        | 3 -> 0x50 (* Timer *)
        | 4 -> 0x58 (* Serial *)
        | 5 -> 0x60 (* Joypad *)
        | _ -> 0x00 (* None *)
      in
      begin match addr with
      | 0 -> st, fetch_decode st
      | n -> st, Instruction.interrupt_service_routine n
      end
    | Enabling -> { st with ime = Enabled }, fetch_decode st
    | Disabled -> st, fetch_decode st
  in
  match instr st with
  | st, Jump, cycles -> st, cycles
  | st, Next, cycles ->
    { st with regs = { st.regs with _PC = st.regs._PC + cycles } }, cycles


let cpu_step st dma hdma ppu =
  (* interrupt or fetch decode execute *)
  let st, mc = fetch_decode_execute st in
  (* timer *)
  let st = { st with timer = Ioregs.Timer.run st.timer mc } in
  (* dma *)
  let st, dma = Dma_unit.OAM.exec_dma st dma mc in
  let st, hdma = Dma_unit.HDMA.exec_dma st hdma mc in
  (* ppu *)
  let ppu = Ppu.process_ppu st.gpu_mem ppu @@ Ppu.dot_of_mc mc in
  st, dma, ppu
  (* w "mainie" bedizemy dodawac st dma ppu do listy debuggera, oraz wyswietlac kolejne piksele z ppu *)

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
  | st, Stop, cycles -> st, cycles
  | st, Halt, cycles -> st, cycles
  | st, Jump, cycles -> st, cycles
  | st, Next, cycles ->
    { st with regs = { st.regs with _PC = st.regs._PC + cycles } }, cycles

let poll_interrupts_halted st =
  let st, addr, act =
    match st.ime with
    | Enabled ->
        begin match Ioregs.IE.get st.ie 0xFFFF land Ioregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
        | 1 -> { st with activity = Running }, 0x40, Running
        | 2 -> { st with activity = Running }, 0x48, Running
        | 3 -> { st with activity = Running }, 0x50, Running
        | 4 -> { st with activity = Running }, 0x58, Running
        | 5 -> { st with activity = Running }, 0x60, Running
        | _ -> st, 0x00, Halted
        end
    | Disabled ->
        begin match Ioregs.IE.get st.ie 0xFFFF land Ioregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
        | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
        | _ -> st, 0x00, Halted
        end
    | Enabling ->
        begin match Ioregs.IE.get st.ie 0xFFFF land Ioregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
        | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
        | _ -> st, 0x00, Halted
        end
  in
  match act, addr with
  | Running, 0x00 -> fetch_decode st st
  | Running, n    -> Instruction.interrupt_service_routine n st
  | Halted,  _    -> st, Halt, 1

let cpu_step st dma hdma ppu =
  match st.activity with
  | Running ->
    (* interrupt or fetch decode execute *)
    let st, mc = fetch_decode_execute st in
    (* timer *)
    (* run div *)
    let st = { st with timer = Ioregs.Timer.run_div st.timer mc } in
    (* run tima *)
    let timer, if_requested = Ioregs.Timer.run_tima st.timer mc in
    let st =
      if if_requested then
        { st with iflag = Ioregs.Interrupts.request_timer st.iflag; timer }
      else
        st
    in
    (* dma *)
    let st, dma = DMAUnit.OAM.exec_dma st dma mc in
    let st, hdma = DMAUnit.VRAM.exec_dma st hdma mc in
    (* ppu *)
    let ppu = Ppu.process_ppu st.gpu_mem ppu @@ Ppu.dot_of_mc mc in
    st, dma, hdma, ppu
    (* w "mainie" bedizemy dodawac st dma ppu do listy debuggera, oraz wyswietlac kolejne piksele z ppu *)
  | Halted ->
    (* check for interrupt *)
    let st, _, mc = poll_interrupts_halted st in
    (* timer *)
    (* run div *)
    let st = { st with timer = Ioregs.Timer.run_div st.timer mc } in
    (* run tima *)
    let timer, if_requested = Ioregs.Timer.run_tima st.timer mc in
    let st =
      if if_requested then
        { st with iflag = Ioregs.Interrupts.request_timer st.iflag; timer }
      else
        st
    in
    (* dma  *)
    let st, dma = DMAUnit.OAM.exec_dma st dma mc in
    (* hdma *)
    let st, hdma = DMAUnit.VRAM.exec_dma st hdma mc in
    (* ppu  *)
    let ppu = Ppu.process_ppu st.gpu_mem ppu @@ Ppu.dot_of_mc mc in
    st, dma, hdma, ppu
  | Stopped n ->
    (* if n = *)

    (* idea - do a set amount of cycles, progress dma hdma and ppu, and then after reaching x cycles change to running *)
    st, dma, hdma, ppu

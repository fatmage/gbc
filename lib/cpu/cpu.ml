open Fetch_decode

let fetch_decode_execute st =
  let instr = fetch_decode st in
  match instr st with
  | st, Jump, cycles -> st, cycles
  | st, Next, cycles ->
    { st with regs = { st.regs with _PC = st.regs._PC + cycles } }, cycles

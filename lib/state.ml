type t =
{ regs: Registers.regfile; flags : Registers.flags;
  rom: Rom.S.t; ram : Ram.S.t; wram : Ram.WRAM.t;
  vram : Ram.VRAM.t; hram : Ram.HRAM.t;
  oam: Oam.S.t; regio: Ioregs.S.t; ie: Iereg.S.t }

let initial =
  {regs = Registers.initial_regfile; flags = Registers.initial_flags;
  rom = Rom.S.empty; ram = Ram.S.empty; wram = Ram.WRAM.empty;
  vram = Ram.VRAM.empty; hram = Ram.HRAM.empty;
  oam = Oam.S.empty; regio = Ioregs.S.empty; ie = Iereg.S.empty }

let set_r8 st r v = { st with regs = Registers.set_r8 st.regs r v }

let set_r16 st rr v = { st with regs = Registers.set_r16 st.regs rr v }

let get_r8 st r = Registers.get_r8 st.regs r

let get_r16 st rr = Registers.get_r16 st.regs rr

let get_flag st f = Registers.get_flag st.flags f

let set_flag st f v = { st with flags = Registers.set_flag st.flags f v }

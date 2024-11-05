type t =
{ regs: Registers.regfile; rom: Rom.S.t; ram : Ram.S.t;
  wram : Ram.WRAM.t; vram : Ram.VRAM.t; hram : Ram.HRAM.t;
  oam: Oam.S.t; regio: Ioregs.S.t; ie: Iereg.S.t }


let initial =
  {regs = Registers.initial; rom = Rom.S.empty; ram = Ram.S.empty;
  wram = Ram.WRAM.empty; vram = Ram.VRAM.empty; hram = Ram.HRAM.empty;
  oam = Oam.S.empty; regio = Ioregs.S.empty; ie = Iereg.S.empty }
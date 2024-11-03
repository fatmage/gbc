
module Oam = struct
  module S = Ram.EightKB
end
module Ioregs = struct
  module S = Ram.EightKB
end
module IEreg = struct
  module S : Addressable.S = struct
    type t = int (* u8 *) list
    let empty = []
    let get t i = List.nth t i
    let set t i v = t
    let in_range i = true
  end
end

type t =
{ regs: Registers.regfile; rom: Rom.S.t; ram : Ram.EightKB.t;
  wram : Ram.EightKB.t; vram : Ram.EightKB.t; hram : Ram.EightKB.t;
  oam: Oam.S.t; regio: Ioregs.S.t; ie: IEreg.S.t }


let initial =
  {regs = Registers.initial; rom = Rom.S.empty; ram = Ram.EightKB.empty;
  wram = Ram.EightKB.empty; vram = Ram.EightKB.empty; hram = Ram.EightKB.empty;
  oam = Oam.S.empty; regio = Ioregs.S.empty; ie = IEreg.S.empty }
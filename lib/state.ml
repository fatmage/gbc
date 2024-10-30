open Inttypes

module Oam = struct
  module S = Ramchunk.S
end
module Ioregs = struct
  module S = Ramchunk.S
end
module IEreg = struct
  module S : Addressable.S = struct
    type t = uint8 list
    let empty = []
    let get t i = List.nth t i
    let set t i v = t
    let in_range i = true
  end
end

type t = { regs: Registers.regfile; rom: Rom.S.t; ram : Ramchunk.S.t;
           wram : Ramchunk.S.t; vram : Ramchunk.S.t; hram : Ramchunk.S.t;
           oam: Oam.S.t; regio: Ioregs.S.t; ie: IEreg.S.t }
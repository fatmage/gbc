open State


(* https://gbdev.io/pandocs/Memory_Map.html
  Memory map:
  0000	3FFF	16 KiB ROM bank 00	From cartridge, usually a fixed bank
  4000	7FFF	16 KiB ROM Bank 01–NN	From cartridge, switchable bank via mapper (if any)
  8000	9FFF	8 KiB Video RAM (VRAM)	In CGB mode, switchable bank 0/1
  A000	BFFF	8 KiB External RAM	From cartridge, switchable bank if any
  C000	CFFF	4 KiB Work RAM (WRAM)
  D000	DFFF	4 KiB Work RAM (WRAM)	In CGB mode, switchable bank 1–7
  E000	FDFF	Echo RAM (mirror of C000–DDFF)	Nintendo says use of this area is prohibited.
  FE00	FE9F	Object attribute memory (OAM)
  FEA0	FEFF	Not Usable	Nintendo says use of this area is prohibited.
  FF00	FF7F	I/O Registers
  FF80	FFFE	High RAM (HRAM)
  FFFF	FFFF	Interrupt Enable register (IE)
 *)

let get t i =
  match i with
  | _ when i <= 0
    -> failwith "Bus error: can't get memory at negative address."
  | _ when i < 0x8000
    -> Rom.S.get t.rom i
  | _ when i < 0xA000
    -> Ramchunk.S.get t.vram i
  | _ when i < 0xC000
    -> Ramchunk.S.get t.ram i
  | _ when i < 0xE000
    -> Ramchunk.S.get t.wram i
  | _ when i >= 0xFE00 && i < 0xFEA0
    -> Oam.S.get t.oam i
  | _ when i >= 0xFF00 && i < 0xFF80
    -> Ioregs.S.get t.regio i
  | _ when i >= 0xFF80 && i < 0xFFFF
    -> Ramchunk.S.get t.hram i
  | _ when i == 0xFFFF
    -> IEreg.S.get t.ie i
  | _
    -> failwith "Bus error: address out of range."


let set t i v =
  match i with
  | i when i <= 0
    -> failwith "Bus error: can't get memory at negative address."
  | i when i < 8192
    -> {t with rom = Rom.S.set t.rom i v}
  | i when i < 16384
    -> {t with ram = Ramchunk.S.set t.ram (i - 8192) v}
  | _
    -> failwith "Bus error: address out of range."
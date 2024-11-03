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

let in_range l v r = l <= v && v <= r

let get8 t i =
  match i with
  | _ when i < 0
    -> failwith "Bus error: can't get memory at negative address."
  | _ when in_range 0x0000 i 0x7FFF (* ROM *)
    -> Rom.S.get t.rom i
  | _ when in_range 0x8000 i 0x9FFF (* VRAM *)
    -> Ram.EightKB.get t.vram i
  | _ when in_range 0xA000 i 0xBFFF (* External RAM *)
    -> Ram.EightKB.get t.ram i
  | _ when in_range 0xC000 i 0xDFFF (* WRAM *)
    -> Ram.EightKB.get t.wram i
  | _ when in_range 0xE000 i 0xFDFF (* Echo RAM *)
    -> Ram.EightKB.get t.wram i
  | _ when in_range 0xFE00 i 0xFE9F (* OAM *)
    -> Oam.S.get t.oam i
  | _ when in_range 0xFF00 i 0xFF7F (* I/O Registers *)
    -> Ioregs.S.get t.regio i
  | _ when in_range 0xFF80 0xFFFE i (* HRAM *)
    -> Ram.EightKB.get t.hram i
  | _ when i == 0xFFFF
    -> IEreg.S.get t.ie i
  | _
    -> failwith "Bus error: address out of range."


let set8 t i v =
  match i with
  | i when i <= 0
    -> failwith "Bus error: can't get memory at negative address."
  | i when i < 8192
    -> {t with rom = Rom.S.set t.rom i v}
  | i when i < 16384
    -> {t with ram = Ram.EightKB.set t.ram (i - 8192) v}
  | _
    -> failwith "Bus error: address out of range."
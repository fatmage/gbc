type interrupts = Disabled | Enabling | Enabled

type t =
{ regs: Regs.regfile; flags : Regs.flags; ime : interrupts;
  rom: Rom.S.t; ram : Ram.S.t; wram : Ram.WRAM.t; vram : Ram.VRAM.t;
  hram : Ram.HRAM.t; oam: Oam.S.t; regio: Ioregs.S.t; ie: Iereg.S.t;
  halted : bool; speed: bool }

  module Bus = struct
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
    FF00	FF7F	addr/O Registers
    FF80	FFFE	High RAM (HRAM)
    FFFF	FFFF	Interrupt Enable register (IE)
    *)

    let in_range l v r = l <= v && v <= r

    let get8 st addr =
      match addr with
      | _ when addr < 0
        -> failwith "Bus error: can't get memory at negative address."
      | _ when in_range 0x0000 addr 0x7FFF (* ROM *)
        -> Rom.S.get st.rom addr
      | _ when in_range 0x8000 addr 0x9FFF (* VRAM *)
        -> Ram.VRAM.get st.vram addr
      | _ when in_range 0xA000 addr 0xBFFF (* External RAM *)
        -> Ram.S.get st.ram addr
      | _ when in_range 0xC000 addr 0xDFFF (* WRAM *)
        -> Ram.WRAM.get st.wram addr
      | _ when in_range 0xE000 addr 0xFDFF (* Echo RAM *)
        -> Ram.WRAM.get st.wram addr
      | _ when in_range 0xFE00 addr 0xFE9F (* OAM *)
        -> Oam.S.get st.oam addr
      | _ when in_range 0xFF00 addr 0xFF7F (* addr/O Registers *)
        -> Ioregs.S.get st.regio addr
      | _ when in_range 0xFF80 0xFFFE addr (* HRAM *)
        -> Ram.HRAM.get st.hram addr
      | _ when addr == 0xFFFF
        -> Iereg.S.get st.ie addr
      | _
        -> failwith "Bus error: address out of range."


    let set8 st addr v =
      match addr with
      | addr when addr <= 0
        -> failwith "Bus error: can't get memory at negative address."
      | addr when addr < 8192
        -> {st with rom = Rom.S.set st.rom addr v}
      | addr when addr < 16384
        -> {st with ram = Ram.S.set st.ram (addr - 8192) v}
      | _
        -> failwith "Bus error: address out of range."

      let get16 st addr =
        let lo, hi = get8 st addr, get8 st (addr + 1) in
        hi lsl 8 lor lo

      let set16 st addr v =
        let lo, hi = v land 0xFF, v land 0xFF00 lsr 8 in
        set8 (set8 st addr lo) (addr + 1) hi

  end

let initial =
  {regs = Regs.initial_regfile; flags = Regs.initial_flags; ime = Disabled;
  rom = Rom.S.empty; ram = Ram.S.empty; wram = Ram.WRAM.empty;
  vram = Ram.VRAM.empty; hram = Ram.HRAM.empty;
  oam = Oam.S.empty; regio = Ioregs.S.empty; ie = Iereg.S.empty;
  halted = false; speed = false }

let set_r8 st r v = { st with regs = Regs.set_r8 st.regs r v }
let set_r16 st rr v = { st with regs = Regs.set_r16 st.regs rr v }
let set_v8 st addr v = Bus.set8 st addr v

let get_r8 st r = Regs.get_r8 st.regs r
let get_r16 st rr = Regs.get_r16 st.regs rr
let get_v8 st addr = Bus.get8 st addr

let get_flag st f = Regs.get_flag st.flags f
let set_flag st f v = { st with flags = Regs.set_flag st.flags f v }
let set_flags st
  ?(z=st.flags.z) ?(n=st.flags.n)
  ?(h=st.flags.h) ?(c=st.flags.c) () =
  { st with flags = { z; n; h; c } }

let get_A st = st.regs._A
let set_A st v = { st with regs = { st.regs with _A = v } }
let get_HL st = st.regs._HL
let set_HL st v = { st with regs = { st.regs with _HL = v } }
let get_HLp st = Bus.get8 st st.regs._HL
let set_HLp st v = Bus.set8 st st.regs._HL v

let get_PC st = st.regs._PC
let set_PC st v = { st with regs = { st.regs with _PC = v } }

let get_SP st = st.regs._SP
let set_SP st v = { st with regs = { st.regs with _SP = v } }
let get_SPp st = Bus.get16 st st.regs._SP
let set_SPp st v = Bus.set16 st st.regs._SP v

let inc_SP st = { st with regs = { st.regs with _SP = st.regs._SP + 2 } }
let dec_SP st = { st with regs = { st.regs with _SP = st.regs._SP - 2 } }

let adv_PC st c = { st with regs = { st.regs with _PC = st.regs._PC + c } }

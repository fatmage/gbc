open Ioregs
open Ram

type interrupts = Disabled | Enabling | Enabled

type t =
  {
    regs: Regs.regfile; flags : Regs.flags;
    rom: Rom.S.t; ram : RAM.t; wram : WRAM.t; gpu_mem : Gpu_mem.t;
    hram : HRAM.t; joypad : Joypad.t; serial: Serial.t;
    timer: Timer.t; iflag : Interrupts.t; audio : Audio.t;
    wave : WavePattern.t; ie: IE.t; halted : bool; ime : interrupts;
  }

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
  FF00	FF7F	I/O Registers
  FF80	FFFE	High RAM (HRAM)
  FFFF	FFFF	Interrupt Enable register (IE)
  *)

  let get8 st addr =
    match addr with
    | _ when Rom.S.in_range addr (* ROM *)
      -> Rom.S.get st.rom addr
    | _ when Gpu_mem.in_range addr (* VRAM, OAM, LCD control, palettes *)
      -> Gpu_mem.get st.gpu_mem addr
    | _ when RAM.in_range addr (* External RAM *)
      -> RAM.get st.ram addr
    | _ when WRAM.in_range addr (* WRAM *)
      -> WRAM.get st.wram addr
    (* ECHO RAM *)
    (* OAM *)
    (* I/O Registers *)
    | _ when Joypad.in_range addr (* Joypad *)
      -> Joypad.get st.joypad addr
    | _ when Serial.in_range addr (* Serial transfer *)
      -> Serial.get st.serial addr
    | _ when Timer.in_range addr (* Timer and divider *)
      -> Timer.get st.timer addr
    | _ when Interrupts.in_range addr (* Interrupts *)
      -> Interrupts.get st.iflag addr
    | _ when Audio.in_range addr (* Audio *)
      -> Audio.get st.audio addr
    | _ when WavePattern.in_range addr (* Wave pattern *)
      -> WavePattern.get st.wave addr
    (* LCD control *)
    (* VRAM bank select *)
    (* 0xFF50 - set to non-zero to disable boot ROM *)
    (* VRAM DMA *)
    (* BG/OBJ palettes *)
    (* WRAM bank select *)
    | _ when HRAM.in_range addr (* HRAM *)
      -> HRAM.get st.hram addr
    | _ when IE.in_range addr (* Interrupt Enable register *)
      -> IE.get st.ie addr
    | _
      -> failwith "Bus error: address out of range."


  let set8 st addr v =
    match addr with
    | _ when Rom.S.in_range addr (* ROM *)
      -> { st with rom = Rom.S.set st.rom addr v }
    | _ when Gpu_mem.VRAM.in_range addr (* VRAM, OAM, LCD control, palettes *)
      -> { st with gpu_mem = Gpu_mem.set st.gpu_mem addr v }
    | _ when RAM.in_range addr (* External RAM *)
      -> { st with ram = RAM.set st.ram addr v }
    | _ when WRAM.in_range addr (* WRAM *)
      -> { st with wram = WRAM.set st.wram addr v }
    (* ECHO RAM *)
    (* OAM *)
    (* I/O Registers *)
    | _ when Joypad.in_range addr (* Joypad *)
      -> { st with joypad = Joypad.set st.joypad addr v }
    | _ when Serial.in_range addr (* Serial transfer *)
      -> { st with serial = Serial.set st.serial addr v }
    | _ when Timer.in_range addr (* Timer and divider *)
      -> { st with timer = Timer.set st.timer addr v }
    | _ when Interrupts.in_range addr (* Interrupts *)
      -> { st with iflag = Interrupts.set st.iflag addr v }
    | _ when Audio.in_range addr (* Audio *)
      -> { st with audio = Audio.set st.audio addr v }
    | _ when WavePattern.in_range addr (* Wave pattern *)
      -> { st with wave = WavePattern.set st.wave addr v }
    (* LCD control *)
    (* VRAM bank select *)
    (* 0xFF50 - set to non-zero to disable boot ROM *)
    (* VRAM DMA *)
    (* BG/OBJ palettes *)
    (* WRAM bank select *)
    | _ when HRAM.in_range addr (* HRAM *)
      -> { st with hram = HRAM.set st.hram addr v }
    | _ when IE.in_range addr (* Interrupt Enable register *)
      -> { st with ie = IE.set st.ie addr v }
    | _
      -> failwith "Bus error: address out of range."

  let get16 st addr =
    let hi, lo = get8 st addr, get8 st (addr + 1) in
    hi lsl 8 lor lo

  let set16 st addr v =
    let hi, lo = v land 0xFF00 lsr 8, v land 0xFF in
    set8 (set8 st addr hi) (addr + 1) lo

end

let initial =
  {
    regs = Regs.initial_regfile; flags = Regs.initial_flags;
    rom = Rom.S.empty; ram = RAM.empty; wram = WRAM.empty;
    gpu_mem = Gpu_mem.empty; hram = HRAM.empty; joypad = Joypad.empty;
    serial = Serial.empty; timer = Timer.empty; iflag = Interrupts.empty;
    audio = Audio.empty; wave = WavePattern.empty;
    ie = IE.empty; halted = false; ime = Disabled
  }

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

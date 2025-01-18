open IOregs



module type S = sig
  type interrupts = Disabled | Enabling | Enabled
  type cpu_activity = Running | Halted of int | Stopped

  module Cartridge : Cartridge.S
  module GPUmem : GPUmem.S

  type t =
  {
    cartridge : Cartridge.t; (* ROM + external RAM *)
    regs: Regs.regfile; wram : RAM.WRAM.t; gpu_mem : GPUmem.t;
    hram : RAM.HRAM.t; joypad : Joypad.t; serial: Serial.t;
    timer: Timer.t; iflag : Interrupts.t; audio : Audio.t;
    wave : WavePattern.t; ie: IE.t; ime : interrupts; activity : cpu_activity;
    dma_oam : DMAState.OAM.t; dma_vram : DMAState.VRAM.t
  }

  module Bus : sig
    val get8 : t -> int -> int
    val get16 : t -> int -> int
    val set8 : t -> int -> int -> t
    val set16 : t -> int -> int -> t
  end
  val initial : t

  val set_r8 : t -> Regs.r8 -> int -> t
  val set_r16 : t -> Regs.r16 -> int -> t
  val set_v8 : t -> int -> int -> t
  val get_r8 : t -> Regs.r8 -> int
  val get_r16 : t -> Regs.r16 -> int
  val get_v8 : t -> int -> int
  val get_flag : t -> Regs.flag -> int
  val set_flag : t -> Regs.flag -> bool -> t
  val set_flags : t -> ?z:bool -> ?n:bool -> ?h:bool -> ?c:bool -> unit -> t
  val get_A : t -> int
  val set_A : t -> int -> t
  val get_HL : t -> int
  val set_HL : t -> int -> t
  val get_HLp : t -> int
  val set_HLp : t -> int -> t
  val get_PC : t -> int
  val set_PC : t -> int -> t
  val get_SP : t -> int
  val set_SP : t -> int -> t
  val get_SPp : t -> int
  val set_SPp : t -> int -> t
  val inc_SP : t -> t
  val dec_SP : t -> t
  val adv_PC : t -> int -> t
  val get_speed : t -> bool
  val request_joypad : t -> t
  val request_serial : t -> t
  val request_timer : t -> t
  val request_LCD : t -> t
  val request_VBlank : t -> t
  val interrupts_pending : t -> int
  val inc_ly : t -> t
  val reset_ly : t -> t
  val get_ly : t -> int
  val update_mode : t -> GPUmode.t -> t
  val change_mode : t -> GPUmode.t -> t
  val get_joypad : t -> int
  val set_joypad : t -> int -> t
  val load_rom : t -> bytes -> t
  val init_dmg : t -> t
  val init_cgb : t -> t
  val mc_to_time : t -> int -> float

end




module Make (M1 : Cartridge.S) (M2 : GPUmem.S) : S = struct
  type interrupts = Disabled | Enabling | Enabled

  type cpu_activity = Running | Halted of int | Stopped

  module Cartridge = M1
  module GPUmem = M2

  type t =
  {
    cartridge : Cartridge.t; (* ROM + external RAM *)
    regs: Regs.regfile; wram : RAM.WRAM.t; gpu_mem : GPUmem.t;
    hram : RAM.HRAM.t; joypad : Joypad.t; serial: Serial.t;
    timer: Timer.t; iflag : Interrupts.t; audio : Audio.t;
    wave : WavePattern.t; ie: IE.t; ime : interrupts; activity : cpu_activity;
    dma_oam : DMAState.OAM.t; dma_vram : DMAState.VRAM.t;
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
      (* Utils.print_hex "Bus get8" addr; *)
      match addr with
      | _ when Cartridge.in_range addr (* ROM + external RAM *)
        -> Cartridge.get st.cartridge addr
      | _ when GPUmem.in_range addr (* VRAM, OAM, LCD control, palettes *)
        -> GPUmem.get st.gpu_mem addr
      (* External RAM *)
      | _ when RAM.WRAM.in_range addr (* WRAM *)
        -> RAM.WRAM.get st.wram addr
      (* ECHO RAM *)
      | _ when RAM.WRAM.in_echo addr
        -> RAM.WRAM.get st.wram (addr - 0x2000)
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
      | _ when RAM.HRAM.in_range addr (* HRAM *)
        ->  RAM.HRAM.get st.hram addr
      | _ when IE.in_range addr (* Interrupt Enable register *)
        -> IE.get st.ie addr
      | _ when DMAState.VRAM.in_range addr
        -> DMAState.VRAM.get st.dma_vram addr
      | _ when DMAState.OAM.in_range addr
        -> DMAState.OAM.get st.dma_oam addr
      | _
        -> Utils.fail_addr "Bus get error: address out of range." addr


    let set8 st addr v =
      Utils.print_hex "set8 addr" addr;
      Utils.print_hex "set8 value" v;
      match addr with
      | _ when Cartridge.in_range addr (* ROM + external cartridge *)
        -> { st with cartridge = Cartridge.set st.cartridge addr v }
      | _ when GPUmem.in_range addr (* VRAM, OAM, LCD control, palettes *)
        -> { st with gpu_mem = GPUmem.set st.gpu_mem addr v }
      (* External RAM *)
      | _ when RAM.WRAM.in_range addr (* WRAM *)
        -> { st with wram = RAM.WRAM.set st.wram addr v }
      (* ECHO RAM *)
      | _ when RAM.WRAM.in_echo addr
        -> { st with wram = RAM.WRAM.set st.wram (addr - 0x2000) v }
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
      | _ when RAM.HRAM.in_range addr (* HRAM *)
        -> { st with hram = RAM.HRAM.set st.hram addr v }
      | _ when IE.in_range addr (* Interrupt Enable register *)
        -> { st with ie = IE.set st.ie addr v }
      | _ when DMAState.VRAM.in_range addr
        -> { st with dma_vram = DMAState.VRAM.set st.dma_vram addr v }
      | _ when DMAState.OAM.in_range addr
        -> { st with dma_oam = DMAState.OAM.set st.dma_oam addr v }
      | _
        -> Utils.fail_addr "Bus set error: address out of range." addr

    let get16 st addr =
      (* if addr = 0xFF69 then get8 st addr else (* That's maybe how palletes work *) *)
      let lo, hi = get8 st addr, get8 st (addr + 1) in
      hi lsl 8 lor lo

    let set16 st addr v =
      let hi, lo = (v land 0xFF00) lsr 8, v land 0xFF in
      set8 (set8 st addr lo) (addr + 1) hi

  end

  let initial =
    {
      cartridge = Cartridge.initial; regs = Regs.initial_regfile; wram = RAM.WRAM.initial;
      gpu_mem = GPUmem.initial; hram = RAM.HRAM.initial; joypad = Joypad.initial;
      serial = Serial.initial; timer = Timer.initial; iflag = Interrupts.initial;
      audio = Audio.initial; wave = WavePattern.initial;
      ie = IE.initial; ime = Disabled; activity = Running;
      dma_oam = DMAState.OAM.initial; dma_vram = DMAState.VRAM.initial;
    }

  let set_r8 st r v = { st with regs = Regs.set_r8 st.regs r v }
  let set_r16 st rr v = { st with regs = Regs.set_r16 st.regs rr v }
  let set_v8 st addr v = Bus.set8 st addr v

  let get_r8 st r = Regs.get_r8 st.regs r
  let get_r16 st rr = Regs.get_r16 st.regs rr
  let get_v8 st addr = Bus.get8 st addr

  let get_flag st f = Regs.get_flag st.regs f
  let set_flag st f v = { st with regs = Regs.set_flag st.regs f v }
  let set_flags st
    ?(z=st.regs.flags.z) ?(n=st.regs.flags.n)
    ?(h=st.regs.flags.h) ?(c=st.regs.flags.c) () =
    { st with regs = { st.regs with flags = { z; n; h; c; low_nibble = st.regs.flags.low_nibble } } }

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

  let get_speed st = Timer.get_speed st.timer

  let request_joypad st = if IE.enabled_joypad st.ie then { st with iflag = Interrupts.request_joypad st.iflag } else st
  let request_serial st = if IE.enabled_serial st.ie then { st with iflag = Interrupts.request_serial st.iflag } else st
  let request_timer st = if IE.enabled_timer st.ie then { st with iflag = Interrupts.request_timer st.iflag } else st
  let request_LCD st = if IE.enabled_LCD st.ie then { st with iflag = Interrupts.request_LCD st.iflag } else st
  let request_VBlank st = if IE.enabled_VBlank st.ie then { st with iflag = Interrupts.request_VBlank st.iflag } else st

  let interrupts_pending st = (IE.get st.ie 0) land (Interrupts.get st.iflag 0)

  let inc_ly st = { st with gpu_mem = GPUmem.inc_ly st.gpu_mem }
  let reset_ly st = { st with gpu_mem = GPUmem.reset_ly st.gpu_mem }
  let get_ly st = GPUmem.get_ly st.gpu_mem

  let update_mode st mode = { st with gpu_mem = GPUmem.update_mode st.gpu_mem mode }
  let change_mode st mode = { st with gpu_mem = GPUmem.change_mode st.gpu_mem mode }

  let get_joypad st = st.joypad
  let set_joypad st v = { st with joypad = IOregs.Joypad.set_input st.joypad v }

  let load_rom st rom = { st with cartridge = Cartridge.load_rom st.cartridge rom }

  let init_dmg st = st

  let init_cgb st =
    {
      st with
      regs = Regs.initial_regfile_cgb; wram = RAM.WRAM.initial;
      gpu_mem = GPUmem.initial; hram = RAM.HRAM.initial; joypad = Joypad.initial;
      serial = Serial.initial; timer = Timer.initial; iflag = Interrupts.initial;
      audio = Audio.initial; wave = WavePattern.initial;
      ie = IE.initial; ime = Disabled; activity = Running;
      dma_oam = DMAState.OAM.initial; dma_vram = DMAState.VRAM.initial;
    }

  let mc_to_time st mc = (float_of_int mc) *. (Timer.tmul st.timer)
end

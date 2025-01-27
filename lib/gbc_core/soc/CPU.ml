
module type S = sig
  module State : State.S
  module PPU : PPU.S
  module Instruction : Instruction.S

  val cpu_step : State.t -> State.t * float * bool

  val init_gb : bytes -> State.t

  val print_registers : State.t -> unit
  val print_interrupts : State.t -> unit
  val print_palettes : State.t -> unit

end

module Make (State : State.S) : S = struct
  module State = State
  module PPU = PPU.Make (State)
  module Instruction = Instruction.Make (State)
  module DMA_OAM = DMAUnit.MakeOAM (State)
  module DMA_VRAM = DMAUnit.MakeOAM (State)
  let match_prefixed st =
    match State.Bus.get8 st (st.regs._PC + 1) with
    | 0x00 -> Instruction.iRLC_r8 B
    | 0x01 -> Instruction.iRLC_r8 C
    | 0x02 -> Instruction.iRLC_r8 D
    | 0x03 -> Instruction.iRLC_r8 E
    | 0x04 -> Instruction.iRLC_r8 H
    | 0x05 -> Instruction.iRLC_r8 L
    | 0x06 -> Instruction.iRLC_HLp
    | 0x07 -> Instruction.iRLC_r8 A
    | 0x08 -> Instruction.iRRC_r8 B
    | 0x09 -> Instruction.iRRC_r8 C
    | 0x0A -> Instruction.iRRC_r8 D
    | 0x0B -> Instruction.iRRC_r8 E
    | 0x0C -> Instruction.iRRC_r8 H
    | 0x0D -> Instruction.iRRC_r8 L
    | 0x0E -> Instruction.iRRC_HLp
    | 0x0F -> Instruction.iRRC_r8 A
    | 0x10 -> Instruction.iRL_r8 B
    | 0x11 -> Instruction.iRL_r8 C
    | 0x12 -> Instruction.iRL_r8 D
    | 0x13 -> Instruction.iRL_r8 E
    | 0x14 -> Instruction.iRL_r8 H
    | 0x15 -> Instruction.iRL_r8 L
    | 0x16 -> Instruction.iRL_HLp
    | 0x17 -> Instruction.iRL_r8 A
    | 0x18 -> Instruction.iRR_r8 B
    | 0x19 -> Instruction.iRR_r8 C
    | 0x1A -> Instruction.iRR_r8 D
    | 0x1B -> Instruction.iRR_r8 E
    | 0x1C -> Instruction.iRR_r8 H
    | 0x1D -> Instruction.iRR_r8 L
    | 0x1E -> Instruction.iRR_HLp
    | 0x1F -> Instruction.iRR_r8 A
    | 0x20 -> Instruction.iSLA_r8 B
    | 0x21 -> Instruction.iSLA_r8 C
    | 0x22 -> Instruction.iSLA_r8 D
    | 0x23 -> Instruction.iSLA_r8 E
    | 0x24 -> Instruction.iSLA_r8 H
    | 0x25 -> Instruction.iSLA_r8 L
    | 0x26 -> Instruction.iSLA_HLp
    | 0x27 -> Instruction.iSLA_r8 A
    | 0x28 -> Instruction.iSRA_r8 B
    | 0x29 -> Instruction.iSRA_r8 C
    | 0x2A -> Instruction.iSRA_r8 D
    | 0x2B -> Instruction.iSRA_r8 E
    | 0x2C -> Instruction.iSRA_r8 H
    | 0x2D -> Instruction.iSRA_r8 L
    | 0x2E -> Instruction.iSRA_HLp
    | 0x2F -> Instruction.iSRA_r8 A
    | 0x30 -> Instruction.iSWAP_r8 B
    | 0x31 -> Instruction.iSWAP_r8 C
    | 0x32 -> Instruction.iSWAP_r8 D
    | 0x33 -> Instruction.iSWAP_r8 E
    | 0x34 -> Instruction.iSWAP_r8 H
    | 0x35 -> Instruction.iSWAP_r8 L
    | 0x36 -> Instruction.iSWAP_HLp
    | 0x37 -> Instruction.iSWAP_r8 A
    | 0x38 -> Instruction.iSRL_r8 B
    | 0x39 -> Instruction.iSRL_r8 C
    | 0x3A -> Instruction.iSRL_r8 D
    | 0x3B -> Instruction.iSRL_r8 E
    | 0x3C -> Instruction.iSRL_r8 H
    | 0x3D -> Instruction.iSRL_r8 L
    | 0x3E -> Instruction.iSRL_HLp
    | 0x3F -> Instruction.iSRL_r8 A
    | 0x40 -> Instruction.iBIT_u3r8 0 B
    | 0x41 -> Instruction.iBIT_u3r8 0 C
    | 0x42 -> Instruction.iBIT_u3r8 0 D
    | 0x43 -> Instruction.iBIT_u3r8 0 E
    | 0x44 -> Instruction.iBIT_u3r8 0 H
    | 0x45 -> Instruction.iBIT_u3r8 0 L
    | 0x46 -> Instruction.iBIT_u3HLp 0
    | 0x47 -> Instruction.iBIT_u3r8 0 A
    | 0x48 -> Instruction.iBIT_u3r8 1 B
    | 0x49 -> Instruction.iBIT_u3r8 1 C
    | 0x4A -> Instruction.iBIT_u3r8 1 D
    | 0x4B -> Instruction.iBIT_u3r8 1 E
    | 0x4C -> Instruction.iBIT_u3r8 1 H
    | 0x4D -> Instruction.iBIT_u3r8 1 L
    | 0x4E -> Instruction.iBIT_u3HLp 1
    | 0x4F -> Instruction.iBIT_u3r8 1 A
    | 0x50 -> Instruction.iBIT_u3r8 2 B
    | 0x51 -> Instruction.iBIT_u3r8 2 C
    | 0x52 -> Instruction.iBIT_u3r8 2 D
    | 0x53 -> Instruction.iBIT_u3r8 2 E
    | 0x54 -> Instruction.iBIT_u3r8 2 H
    | 0x55 -> Instruction.iBIT_u3r8 2 L
    | 0x56 -> Instruction.iBIT_u3HLp 2
    | 0x57 -> Instruction.iBIT_u3r8 2 A
    | 0x58 -> Instruction.iBIT_u3r8 3 B
    | 0x59 -> Instruction.iBIT_u3r8 3 C
    | 0x5A -> Instruction.iBIT_u3r8 3 D
    | 0x5B -> Instruction.iBIT_u3r8 3 E
    | 0x5C -> Instruction.iBIT_u3r8 3 H
    | 0x5D -> Instruction.iBIT_u3r8 3 L
    | 0x5E -> Instruction.iBIT_u3HLp 3
    | 0x5F -> Instruction.iBIT_u3r8 3 A
    | 0x60 -> Instruction.iBIT_u3r8 4 B
    | 0x61 -> Instruction.iBIT_u3r8 4 C
    | 0x62 -> Instruction.iBIT_u3r8 4 D
    | 0x63 -> Instruction.iBIT_u3r8 4 E
    | 0x64 -> Instruction.iBIT_u3r8 4 H
    | 0x65 -> Instruction.iBIT_u3r8 4 L
    | 0x66 -> Instruction.iBIT_u3HLp 4
    | 0x67 -> Instruction.iBIT_u3r8 4 A
    | 0x68 -> Instruction.iBIT_u3r8 5 B
    | 0x69 -> Instruction.iBIT_u3r8 5 C
    | 0x6A -> Instruction.iBIT_u3r8 5 D
    | 0x6B -> Instruction.iBIT_u3r8 5 E
    | 0x6C -> Instruction.iBIT_u3r8 5 H
    | 0x6D -> Instruction.iBIT_u3r8 5 L
    | 0x6E -> Instruction.iBIT_u3HLp 5
    | 0x6F -> Instruction.iBIT_u3r8 5 A
    | 0x70 -> Instruction.iBIT_u3r8 6 B
    | 0x71 -> Instruction.iBIT_u3r8 6 C
    | 0x72 -> Instruction.iBIT_u3r8 6 D
    | 0x73 -> Instruction.iBIT_u3r8 6 E
    | 0x74 -> Instruction.iBIT_u3r8 6 H
    | 0x75 -> Instruction.iBIT_u3r8 6 L
    | 0x76 -> Instruction.iBIT_u3HLp 6
    | 0x77 -> Instruction.iBIT_u3r8 6 A
    | 0x78 -> Instruction.iBIT_u3r8 7 B
    | 0x79 -> Instruction.iBIT_u3r8 7 C
    | 0x7A -> Instruction.iBIT_u3r8 7 D
    | 0x7B -> Instruction.iBIT_u3r8 7 E
    | 0x7C -> Instruction.iBIT_u3r8 7 H
    | 0x7D -> Instruction.iBIT_u3r8 7 L
    | 0x7E -> Instruction.iBIT_u3HLp 7
    | 0x7F -> Instruction.iBIT_u3r8 7 A
    | 0x80 -> Instruction.iRES_u3r8 0 B
    | 0x81 -> Instruction.iRES_u3r8 0 C
    | 0x82 -> Instruction.iRES_u3r8 0 D
    | 0x83 -> Instruction.iRES_u3r8 0 E
    | 0x84 -> Instruction.iRES_u3r8 0 H
    | 0x85 -> Instruction.iRES_u3r8 0 L
    | 0x86 -> Instruction.iRES_u3HLp 0
    | 0x87 -> Instruction.iRES_u3r8 0 A
    | 0x88 -> Instruction.iRES_u3r8 1 B
    | 0x89 -> Instruction.iRES_u3r8 1 C
    | 0x8A -> Instruction.iRES_u3r8 1 D
    | 0x8B -> Instruction.iRES_u3r8 1 E
    | 0x8C -> Instruction.iRES_u3r8 1 H
    | 0x8D -> Instruction.iRES_u3r8 1 L
    | 0x8E -> Instruction.iRES_u3HLp 1
    | 0x8F -> Instruction.iRES_u3r8 1 A
    | 0x90 -> Instruction.iRES_u3r8 2 B
    | 0x91 -> Instruction.iRES_u3r8 2 C
    | 0x92 -> Instruction.iRES_u3r8 2 D
    | 0x93 -> Instruction.iRES_u3r8 2 E
    | 0x94 -> Instruction.iRES_u3r8 2 H
    | 0x95 -> Instruction.iRES_u3r8 2 L
    | 0x96 -> Instruction.iRES_u3HLp 2
    | 0x97 -> Instruction.iRES_u3r8 2 A
    | 0x98 -> Instruction.iRES_u3r8 3 B
    | 0x99 -> Instruction.iRES_u3r8 3 C
    | 0x9A -> Instruction.iRES_u3r8 3 D
    | 0x9B -> Instruction.iRES_u3r8 3 E
    | 0x9C -> Instruction.iRES_u3r8 3 H
    | 0x9D -> Instruction.iRES_u3r8 3 L
    | 0x9E -> Instruction.iRES_u3HLp 3
    | 0x9F -> Instruction.iRES_u3r8 3 A
    | 0xA0 -> Instruction.iRES_u3r8 4 B
    | 0xA1 -> Instruction.iRES_u3r8 4 C
    | 0xA2 -> Instruction.iRES_u3r8 4 D
    | 0xA3 -> Instruction.iRES_u3r8 4 E
    | 0xA4 -> Instruction.iRES_u3r8 4 H
    | 0xA5 -> Instruction.iRES_u3r8 4 L
    | 0xA6 -> Instruction.iRES_u3HLp 4
    | 0xA7 -> Instruction.iRES_u3r8 4 A
    | 0xA8 -> Instruction.iRES_u3r8 5 B
    | 0xA9 -> Instruction.iRES_u3r8 5 C
    | 0xAA -> Instruction.iRES_u3r8 5 D
    | 0xAB -> Instruction.iRES_u3r8 5 E
    | 0xAC -> Instruction.iRES_u3r8 5 H
    | 0xAD -> Instruction.iRES_u3r8 5 L
    | 0xAE -> Instruction.iRES_u3HLp 5
    | 0xAF -> Instruction.iRES_u3r8 5 A
    | 0xB0 -> Instruction.iRES_u3r8 6 B
    | 0xB1 -> Instruction.iRES_u3r8 6 C
    | 0xB2 -> Instruction.iRES_u3r8 6 D
    | 0xB3 -> Instruction.iRES_u3r8 6 E
    | 0xB4 -> Instruction.iRES_u3r8 6 H
    | 0xB5 -> Instruction.iRES_u3r8 6 L
    | 0xB6 -> Instruction.iRES_u3HLp 6
    | 0xB7 -> Instruction.iRES_u3r8 6 A
    | 0xB8 -> Instruction.iRES_u3r8 7 B
    | 0xB9 -> Instruction.iRES_u3r8 7 C
    | 0xBA -> Instruction.iRES_u3r8 7 D
    | 0xBB -> Instruction.iRES_u3r8 7 E
    | 0xBC -> Instruction.iRES_u3r8 7 H
    | 0xBD -> Instruction.iRES_u3r8 7 L
    | 0xBE -> Instruction.iRES_u3HLp 7
    | 0xBF -> Instruction.iRES_u3r8 7 A
    | 0xC0 -> Instruction.iSET_u3r8 0 B
    | 0xC1 -> Instruction.iSET_u3r8 0 C
    | 0xC2 -> Instruction.iSET_u3r8 0 D
    | 0xC3 -> Instruction.iSET_u3r8 0 E
    | 0xC4 -> Instruction.iSET_u3r8 0 H
    | 0xC5 -> Instruction.iSET_u3r8 0 L
    | 0xC6 -> Instruction.iSET_u3HLp 0
    | 0xC7 -> Instruction.iSET_u3r8 0 A
    | 0xC8 -> Instruction.iSET_u3r8 1 B
    | 0xC9 -> Instruction.iSET_u3r8 1 C
    | 0xCA -> Instruction.iSET_u3r8 1 D
    | 0xCB -> Instruction.iSET_u3r8 1 E
    | 0xCC -> Instruction.iSET_u3r8 1 H
    | 0xCD -> Instruction.iSET_u3r8 1 L
    | 0xCE -> Instruction.iSET_u3HLp 1
    | 0xCF -> Instruction.iSET_u3r8 1 A
    | 0xD0 -> Instruction.iSET_u3r8 2 B
    | 0xD1 -> Instruction.iSET_u3r8 2 C
    | 0xD2 -> Instruction.iSET_u3r8 2 D
    | 0xD3 -> Instruction.iSET_u3r8 2 E
    | 0xD4 -> Instruction.iSET_u3r8 2 H
    | 0xD5 -> Instruction.iSET_u3r8 2 L
    | 0xD6 -> Instruction.iSET_u3HLp 2
    | 0xD7 -> Instruction.iSET_u3r8 2 A
    | 0xD8 -> Instruction.iSET_u3r8 3 B
    | 0xD9 -> Instruction.iSET_u3r8 3 C
    | 0xDA -> Instruction.iSET_u3r8 3 D
    | 0xDB -> Instruction.iSET_u3r8 3 E
    | 0xDC -> Instruction.iSET_u3r8 3 H
    | 0xDD -> Instruction.iSET_u3r8 3 L
    | 0xDE -> Instruction.iSET_u3HLp 3
    | 0xDF -> Instruction.iSET_u3r8 3 A
    | 0xE0 -> Instruction.iSET_u3r8 4 B
    | 0xE1 -> Instruction.iSET_u3r8 4 C
    | 0xE2 -> Instruction.iSET_u3r8 4 D
    | 0xE3 -> Instruction.iSET_u3r8 4 E
    | 0xE4 -> Instruction.iSET_u3r8 4 H
    | 0xE5 -> Instruction.iSET_u3r8 4 L
    | 0xE6 -> Instruction.iSET_u3HLp 4
    | 0xE7 -> Instruction.iSET_u3r8 4 A
    | 0xE8 -> Instruction.iSET_u3r8 5 B
    | 0xE9 -> Instruction.iSET_u3r8 5 C
    | 0xEA -> Instruction.iSET_u3r8 5 D
    | 0xEB -> Instruction.iSET_u3r8 5 E
    | 0xEC -> Instruction.iSET_u3r8 5 H
    | 0xED -> Instruction.iSET_u3r8 5 L
    | 0xEE -> Instruction.iSET_u3HLp 5
    | 0xEF -> Instruction.iSET_u3r8 5 A
    | 0xF0 -> Instruction.iSET_u3r8 6 B
    | 0xF1 -> Instruction.iSET_u3r8 6 C
    | 0xF2 -> Instruction.iSET_u3r8 6 D
    | 0xF3 -> Instruction.iSET_u3r8 6 E
    | 0xF4 -> Instruction.iSET_u3r8 6 H
    | 0xF5 -> Instruction.iSET_u3r8 6 L
    | 0xF6 -> Instruction.iSET_u3HLp 6
    | 0xF7 -> Instruction.iSET_u3r8 6 A
    | 0xF8 -> Instruction.iSET_u3r8 7 B
    | 0xF9 -> Instruction.iSET_u3r8 7 C
    | 0xFA -> Instruction.iSET_u3r8 7 D
    | 0xFB -> Instruction.iSET_u3r8 7 E
    | 0xFC -> Instruction.iSET_u3r8 7 H
    | 0xFD -> Instruction.iSET_u3r8 7 L
    | 0xFE -> Instruction.iSET_u3HLp 7
    | 0xFF -> Instruction.iSET_u3r8 7 A
    | _ -> Instruction.iNOP

  let fetch_decode (st : State.t) : Instruction.instruction * int =
    print_endline "========= FETCH_DECODE START =========";
    Utils.print_hex "3 next values at PC" st.regs._PC;
    Utils.value_hex (State.Bus.get8 st st.regs._PC);
    Utils.value_hex (State.Bus.get8 st (st.regs._PC + 1));
    Utils.value_hex (State.Bus.get8 st (st.regs._PC + 2));
    State.print_registers st;
    State.print_interrupts st;
    Utils.print_hex "LCDC" st.gpu_mem.lcd_regs.lcdc;
    Utils.print_hex "STAT" st.gpu_mem.lcd_regs.stat;
    let _ = if st.regs._SP != 0xFFFF then
    Utils.print_hex "Value at SP" @@ State.get_SPp st else () in
    Utils.print_hex "Value at HL" @@ State.get_HLp st;
    Utils.print_hex "LY" st.gpu_mem.lcd_regs.ly;
    (* let _ = if st.regs._PC = 0x073E then read_line () else "" in *)

    match State.Bus.get8 st st.regs._PC with
    | 0x00 -> Instruction.iNOP, 1
    | 0x01 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 BC n16, 3
    | 0x02 -> Instruction.iLD_r16pA BC, 1
    | 0x03 -> Instruction.iINC_r16 BC, 1
    | 0x04 -> Instruction.iINC_r8 B, 1
    | 0x05 -> Instruction.iDEC_r8 B, 1
    | 0x06 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 B n8, 2
    | 0x07 -> Instruction.iRLCA, 1
    | 0x08 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_n16pSP n16, 3
    | 0x09 -> Instruction.iADD_HLr16 BC, 1
    | 0x0A -> Instruction.iLD_Ar16p BC, 1
    | 0x0B -> Instruction.iDEC_r16 BC, 1
    | 0x0C -> Instruction.iINC_r8 C, 1
    | 0x0D -> Instruction.iDEC_r8 C, 1
    | 0x0E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 C n8, 2
    | 0x0F -> Instruction.iRRCA, 1
    | 0x10 -> Instruction.iSTOP st
    | 0x11 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 DE n16, 3
    | 0x12 -> Instruction.iLD_r16pA DE, 1
    | 0x13 -> Instruction.iINC_r16 DE, 1
    | 0x14 -> Instruction.iINC_r8 D, 1
    | 0x15 -> Instruction.iDEC_r8 D, 1
    | 0x16 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 D n8, 2
    | 0x17 -> Instruction.iRLA, 1
    | 0x18 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_n8 n8, 2
    | 0x19 -> Instruction.iADD_HLr16 DE, 1
    | 0x1A -> Instruction.iLD_Ar16p DE, 1
    | 0x1B -> Instruction.iDEC_r16 DE, 1
    | 0x1C -> Instruction.iINC_r8 E, 1
    | 0x1D -> Instruction.iDEC_r8 E, 1
    | 0x1E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 E n8, 2
    | 0x1F -> Instruction.iRRA, 1
    | 0x20 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cnz n8, 2
    | 0x21 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 HL n16, 3
    | 0x22 -> Instruction.iLD_HLIpA, 1
    | 0x23 -> Instruction.iINC_r16 HL, 1
    | 0x24 -> Instruction.iINC_r8 H, 1
    | 0x25 -> Instruction.iDEC_r8 H, 1
    | 0x26 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 H n8, 2
    | 0x27 -> Instruction.iDAA, 1
    | 0x28 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cz n8, 2
    | 0x29 -> Instruction.iADD_HLr16 HL, 1
    | 0x2A -> Instruction.iLD_AHLIp, 1
    | 0x2B -> Instruction.iDEC_r16 HL, 1
    | 0x2C -> Instruction.iINC_r8 L, 1
    | 0x2D -> Instruction.iDEC_r8 L, 1
    | 0x2E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 L n8, 2
    | 0x2F -> Instruction.iCPL, 1
    | 0x30 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cnc n8, 2
    | 0x31 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_SPn16 n16, 3
    | 0x32 -> Instruction.iLD_HLDpA, 1
    | 0x33 -> Instruction.iINC_r16 SP, 1
    | 0x34 -> Instruction.iINC_HLp, 1
    | 0x35 -> Instruction.iDEC_HLp, 1
    | 0x36 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_HLpn8 n8, 2
    | 0x37 -> Instruction.iSCF, 1
    | 0x38 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cc n8, 2
    | 0x39 -> Instruction.iADD_HLSP, 1
    | 0x3A -> Instruction.iLD_AHLDp, 1
    | 0x3B -> Instruction.iDEC_r16 SP, 1
    | 0x3C -> Instruction.iINC_r8 A, 1
    | 0x3D -> Instruction.iDEC_r8 A, 1
    | 0x3E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 A n8, 2
    | 0x3F -> Instruction.iCCF, 1
    | 0x40 -> Instruction.iLD_rr8 B B, 1
    | 0x41 -> Instruction.iLD_rr8 B C, 1
    | 0x42 -> Instruction.iLD_rr8 B D, 1
    | 0x43 -> Instruction.iLD_rr8 B E, 1
    | 0x44 -> Instruction.iLD_rr8 B H, 1
    | 0x45 -> Instruction.iLD_rr8 B L, 1
    | 0x46 -> Instruction.iLD_r8HLp B, 1
    | 0x47 -> Instruction.iLD_rr8 B A, 1
    | 0x48 -> Instruction.iLD_rr8 C B, 1
    | 0x49 -> Instruction.iLD_rr8 C C, 1
    | 0x4A -> Instruction.iLD_rr8 C D, 1
    | 0x4B -> Instruction.iLD_rr8 C E, 1
    | 0x4C -> Instruction.iLD_rr8 C H, 1
    | 0x4D -> Instruction.iLD_rr8 C L, 1
    | 0x4E -> Instruction.iLD_r8HLp C, 1
    | 0x4F -> Instruction.iLD_rr8 C A, 1
    | 0x50 -> Instruction.iLD_rr8 D B, 1
    | 0x51 -> Instruction.iLD_rr8 D C, 1
    | 0x52 -> Instruction.iLD_rr8 D D, 1
    | 0x53 -> Instruction.iLD_rr8 D E, 1
    | 0x54 -> Instruction.iLD_rr8 D H, 1
    | 0x55 -> Instruction.iLD_rr8 D L, 1
    | 0x56 -> Instruction.iLD_r8HLp D, 1
    | 0x57 -> Instruction.iLD_rr8 D A, 1
    | 0x58 -> Instruction.iLD_rr8 E B, 1
    | 0x59 -> Instruction.iLD_rr8 E C, 1
    | 0x5A -> Instruction.iLD_rr8 E D, 1
    | 0x5B -> Instruction.iLD_rr8 E E, 1
    | 0x5C -> Instruction.iLD_rr8 E H, 1
    | 0x5D -> Instruction.iLD_rr8 E L, 1
    | 0x5E -> Instruction.iLD_r8HLp E, 1
    | 0x5F -> Instruction.iLD_rr8 E A, 1
    | 0x60 -> Instruction.iLD_rr8 H B, 1
    | 0x61 -> Instruction.iLD_rr8 H C, 1
    | 0x62 -> Instruction.iLD_rr8 H D, 1
    | 0x63 -> Instruction.iLD_rr8 H E, 1
    | 0x64 -> Instruction.iLD_rr8 H H, 1
    | 0x65 -> Instruction.iLD_rr8 H L, 1
    | 0x66 -> Instruction.iLD_r8HLp H, 1
    | 0x67 -> Instruction.iLD_rr8 H A, 1
    | 0x68 -> Instruction.iLD_rr8 L B, 1
    | 0x69 -> Instruction.iLD_rr8 L C, 1
    | 0x6A -> Instruction.iLD_rr8 L D, 1
    | 0x6B -> Instruction.iLD_rr8 L E, 1
    | 0x6C -> Instruction.iLD_rr8 L H, 1
    | 0x6D -> Instruction.iLD_rr8 L L, 1
    | 0x6E -> Instruction.iLD_r8HLp L, 1
    | 0x6F -> Instruction.iLD_rr8 L A, 1
    | 0x70 -> Instruction.iLD_HLpr8 B, 1
    | 0x71 -> Instruction.iLD_HLpr8 C, 1
    | 0x72 -> Instruction.iLD_HLpr8 D, 1
    | 0x73 -> Instruction.iLD_HLpr8 E, 1
    | 0x74 -> Instruction.iLD_HLpr8 H, 1
    | 0x75 -> Instruction.iLD_HLpr8 L, 1
    | 0x76 -> Instruction.iHALT, 1
    | 0x77 -> Instruction.iLD_HLpr8 A, 1
    | 0x78 -> Instruction.iLD_rr8 A B, 1
    | 0x79 -> Instruction.iLD_rr8 A C, 1
    | 0x7A -> Instruction.iLD_rr8 A D, 1
    | 0x7B -> Instruction.iLD_rr8 A E, 1
    | 0x7C -> Instruction.iLD_rr8 A H, 1
    | 0x7D -> Instruction.iLD_rr8 A L, 1
    | 0x7E -> Instruction.iLD_r8HLp A, 1
    | 0x7F -> Instruction.iLD_rr8 A A, 1
    | 0x80 -> Instruction.iADD_Ar8 B, 1
    | 0x81 -> Instruction.iADD_Ar8 C, 1
    | 0x82 -> Instruction.iADD_Ar8 D, 1
    | 0x83 -> Instruction.iADD_Ar8 E, 1
    | 0x84 -> Instruction.iADD_Ar8 H, 1
    | 0x85 -> Instruction.iADD_Ar8 L, 1
    | 0x86 -> Instruction.iADD_AHLp, 1
    | 0x87 -> Instruction.iADD_Ar8 A, 1
    | 0x88 -> Instruction.iADC_Ar8 B, 1
    | 0x89 -> Instruction.iADC_Ar8 C, 1
    | 0x8A -> Instruction.iADC_Ar8 D, 1
    | 0x8B -> Instruction.iADC_Ar8 E, 1
    | 0x8C -> Instruction.iADC_Ar8 H, 1
    | 0x8D -> Instruction.iADC_Ar8 L, 1
    | 0x8E -> Instruction.iADC_AHLp, 1
    | 0x8F -> Instruction.iADC_Ar8 A, 1
    | 0x90 -> Instruction.iSUB_Ar8 B, 1
    | 0x91 -> Instruction.iSUB_Ar8 C, 1
    | 0x92 -> Instruction.iSUB_Ar8 D, 1
    | 0x93 -> Instruction.iSUB_Ar8 E, 1
    | 0x94 -> Instruction.iSUB_Ar8 H, 1
    | 0x95 -> Instruction.iSUB_Ar8 L, 1
    | 0x96 -> Instruction.iSUB_AHLp, 1
    | 0x97 -> Instruction.iSUB_Ar8 A, 1
    | 0x98 -> Instruction.iSBC_Ar8 B, 1
    | 0x99 -> Instruction.iSBC_Ar8 C, 1
    | 0x9A -> Instruction.iSBC_Ar8 D, 1
    | 0x9B -> Instruction.iSBC_Ar8 E, 1
    | 0x9C -> Instruction.iSBC_Ar8 H, 1
    | 0x9D -> Instruction.iSBC_Ar8 L, 1
    | 0x9E -> Instruction.iSBC_AHLp, 1
    | 0x9F -> Instruction.iSBC_Ar8 A, 1
    | 0xA0 -> Instruction.iAND_Ar8 B, 1
    | 0xA1 -> Instruction.iAND_Ar8 C, 1
    | 0xA2 -> Instruction.iAND_Ar8 D, 1
    | 0xA3 -> Instruction.iAND_Ar8 E, 1
    | 0xA4 -> Instruction.iAND_Ar8 H, 1
    | 0xA5 -> Instruction.iAND_Ar8 L, 1
    | 0xA6 -> Instruction.iAND_AHLp, 1
    | 0xA7 -> Instruction.iAND_Ar8 A, 1
    | 0xA8 -> Instruction.iXOR_Ar8 B, 1
    | 0xA9 -> Instruction.iXOR_Ar8 C, 1
    | 0xAA -> Instruction.iXOR_Ar8 D, 1
    | 0xAB -> Instruction.iXOR_Ar8 E, 1
    | 0xAC -> Instruction.iXOR_Ar8 H, 1
    | 0xAD -> Instruction.iXOR_Ar8 L, 1
    | 0xAE -> Instruction.iXOR_AHLp, 1
    | 0xAF -> Instruction.iXOR_Ar8 A, 1
    | 0xB0 -> Instruction.iOR_Ar8 B, 1
    | 0xB1 -> Instruction.iOR_Ar8 C, 1
    | 0xB2 -> Instruction.iOR_Ar8 D, 1
    | 0xB3 -> Instruction.iOR_Ar8 E, 1
    | 0xB4 -> Instruction.iOR_Ar8 H, 1
    | 0xB5 -> Instruction.iOR_Ar8 L, 1
    | 0xB6 -> Instruction.iOR_AHLp, 1
    | 0xB7 -> Instruction.iOR_Ar8 A, 1
    | 0xB8 -> Instruction.iCP_Ar8 B, 1
    | 0xB9 -> Instruction.iCP_Ar8 C, 1
    | 0xBA -> Instruction.iCP_Ar8 D, 1
    | 0xBB -> Instruction.iCP_Ar8 E, 1
    | 0xBC -> Instruction.iCP_Ar8 H, 1
    | 0xBD -> Instruction.iCP_Ar8 L, 1
    | 0xBE -> Instruction.iCP_AHLp, 1
    | 0xBF -> Instruction.iCP_Ar8 A, 1
    | 0xC0 -> Instruction.iRET_c Cnz, 1
    | 0xC1 -> Instruction.iPOP_r16 BC, 1
    | 0xC2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cnz n16, 3
    | 0xC3 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_n16 n16, 3
    | 0xC4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cnz n16, 3
    | 0xC5 -> Instruction.iPUSH_r16 BC, 1
    | 0xC6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADD_An8 n8, 2
    | 0xC7 -> Instruction.iRST_vec 0x00, 1
    | 0xC8 -> Instruction.iRET_c Cz, 1
    | 0xC9 -> Instruction.iRET, 1
    | 0xCA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cz n16, 3
    (* | 0xCB ->  prefix CB *)
    | 0xCC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cz n16, 3
    | 0xCD -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_n16 n16, 3
    | 0xCE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADC_An8 n8, 2
    | 0xCF -> Instruction.iRST_vec 0x08, 1
    | 0xD0 -> Instruction.iRET_c Cnc, 1
    | 0xD1 -> Instruction.iPOP_r16 DE, 1
    | 0xD2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cnc n16, 3
    (* | 0xD3 ->  iNOP*)
    | 0xD4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cnc n16, 3
    | 0xD5 -> Instruction.iPUSH_r16 DE, 1
    | 0xD6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iSUB_An8 n8, 2
    | 0xD7 -> Instruction.iRST_vec 0x10, 1
    | 0xD8 -> Instruction.iRET_c Cc, 1
    | 0xD9 -> Instruction.iRETI, 1
    | 0xDA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cc n16, 3
    (* | 0xDB -> Instruction.iNOP  *)
    | 0xDC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cc n16, 3
    (* | 0xDD -> Instruction.iNOP  *)
    | 0xDE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iSBC_An8 n8, 2
    | 0xDF -> Instruction.iRST_vec 0x18, 1
    | 0xE0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLDH_n16pA n8, 2
    | 0xE1 -> Instruction.iPOP_r16 HL, 1
    | 0xE2 -> Instruction.iLDH_CpA, 1
    (* | 0xE3 -> Instruction.iNOP  *)
    (* | 0xE4 -> Instruction.iNOP  *)
    | 0xE5 -> Instruction.iPUSH_r16 HL, 1
    | 0xE6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iAND_An8 n8, 2
    | 0xE7 -> Instruction.iRST_vec 0x20, 1
    | 0xE8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADD_SPe8 n8, 2
    | 0xE9 -> Instruction.iJP_HL, 1
    | 0xEA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_n16pA n16, 3
    (* | 0xEB -> Instruction.iNOP  *)
    (* | 0xEC -> Instruction.iNOP  *)
    (* | 0xED -> Instruction.iNOP  *)
    | 0xEE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iXOR_An8 n8, 2
    | 0xEF -> Instruction.iRST_vec 0x28, 1
    | 0xF0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLDH_An16p n8, 2
    | 0xF1 -> Instruction.iPOP_AF, 1
    | 0xF2 -> Instruction.iLDH_ACp, 1
    | 0xF3 -> Instruction.iDI, 1
    (* | 0xF4 -> Instruction.iNOP  *)
    | 0xF5 -> Instruction.iPUSH_AF, 1
    | 0xF6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iOR_An8 n8, 2
    | 0xF7 -> Instruction.iRST_vec 0x30, 1
    | 0xF8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_HLSPe8 n8, 2
    | 0xF9 -> Instruction.iLD_SPHL, 1
    | 0xFA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_An16p n16, 3
    | 0xFB -> Instruction.iEI, 1
    (* | 0xFC -> Instruction.iNOP  *)
    (* | 0xFD -> Instruction.iNOP  *)
    | 0xFE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iCP_An8 n8, 2
    | 0xFF -> Instruction.iRST_vec 0x38, 1
    (* CB-prefixed *)
    | 0xCB -> match_prefixed st, 2
    | _    -> Instruction.iNOP, 1

  let first_set_bit =
    let rec aux n m k =
      match n land m with
      | 0 -> aux n (m * 2) (k + 1)
      | _ -> k
    in
    function
    | 0 -> 0
    | n -> aux n 1 1

  let execute (instr : Instruction.instruction) st length =
    match instr st with
    | st, Next, cycles ->
      State.adv_PC st length, cycles
    | st, RelJump, cycles ->
      State.adv_PC st length, cycles
    | st, Jump, cycles -> st, cycles

  let fetch_decode_execute (st : State.t) =
    let st, (instr, length) =
      match st.ime with
      | Enabled  ->
        let st, addr =
          match first_set_bit @@ (IOregs.IE.get st.ie 0xFFFF) land (IOregs.Interrupts.get st.iflag 0xFF0F) with
          | 1 -> { st with iflag = IOregs.Interrupts.handled_VBlank st.iflag }, 0x40 (* VBlank *)
          | 2 -> { st with iflag = IOregs.Interrupts.handled_LCD st.iflag }, 0x48 (* LCD *)
          | 3 -> { st with iflag = IOregs.Interrupts.handled_timer st.iflag }, 0x50 (* Timer *)
          | 4 -> { st with iflag = IOregs.Interrupts.handled_serial st.iflag }, 0x58 (* Serial *)
          | 5 -> { st with iflag = IOregs.Interrupts.handled_joypad st.iflag }, 0x60 (* Joypad *)
          | _ -> st, 0x00 (* None *)
        in
        begin match addr with
        | 0 -> st, fetch_decode st
        | n -> Utils.print_hex "Servicujemy interrupt" n;
        st, (Instruction.interrupt_service_routine n, 0)
        end
      | Enabling -> { st with ime = Enabled }, fetch_decode st
      | Disabled -> st, fetch_decode st
    in
    execute instr st length

  let poll_interrupts_halted (st : State.t) =
    (* print_endline "pollujemy interrupty";
    Utils.print_hex "LCDC" st.gpu_mem.lcd_regs.lcdc;
    Utils.print_hex "STAT" st.gpu_mem.lcd_regs.stat;
    Utils.print_hex "LY" st.gpu_mem.lcd_regs.ly;
    Utils.print_hex "LYC" st.gpu_mem.lcd_regs.lyc;
    State.print_interrupts st; *)
    let st, addr, (act : State.cpu_activity) =
      match st.ime with
      | Enabled ->
          begin match first_set_bit @@ (IOregs.IE.get st.ie 0xFFFF) land (IOregs.Interrupts.get st.iflag 0xFF0F) with
          | 1 -> { st with activity = Running; iflag = IOregs.Interrupts.handled_VBlank st.iflag }, 0x40, Running
          | 2 -> { st with activity = Running; iflag = IOregs.Interrupts.handled_LCD st.iflag    }, 0x48, Running
          | 3 -> { st with activity = Running; iflag = IOregs.Interrupts.handled_timer st.iflag  }, 0x50, Running
          | 4 -> { st with activity = Running; iflag = IOregs.Interrupts.handled_serial st.iflag }, 0x58, Running
          | 5 -> { st with activity = Running; iflag = IOregs.Interrupts.handled_joypad st.iflag }, 0x60, Running
          | _ -> st, 0x00, st.activity
          end
      | Disabled ->
          begin match first_set_bit @@ (IOregs.IE.get st.ie 0xFFFF) land (IOregs.Interrupts.get st.iflag 0xFF0F) with
          | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
          | _ -> st, 0x00, st.activity
          end
      | Enabling ->
          begin match first_set_bit @@ (IOregs.IE.get st.ie 0xFFFF) land (IOregs.Interrupts.get st.iflag 0xFF0F) with
          | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
          | _ ->  { st with ime = Enabled }, 0x00, st.activity
          end
    in
    match act, addr with
    | Running, 0x00  -> fetch_decode_execute st
    | Running, n     -> execute (Instruction.interrupt_service_routine n) st 0
    | Halted (-1), _ -> st, 1
    | Halted 1, _    ->     Utils.print_hex "3 next values at PC when exiting halt:" st.regs._PC;
    Utils.value_hex (State.Bus.get8 st st.regs._PC);
    Utils.value_hex (State.Bus.get8 st (st.regs._PC + 1));
    Utils.value_hex (State.Bus.get8 st (st.regs._PC + 2));{ st with activity = Running }, 1
    | Halted n, _    -> { st with activity = Halted (n-1) }, 1


  let cpu_step (st : State.t) =
    match st.activity with
    | Running ->
      (* interrupt or fetch decode execute *)
      let st, mc = fetch_decode_execute st in
      (* timer *)
      (* run div *)
      let st = { st with timer = IOregs.Timer.run_div st.timer mc } in
      (* run tima *)
      let timer, if_requested = IOregs.Timer.run_tima st.timer mc in
      let st =
        if if_requested then
          { st with iflag = IOregs.Interrupts.request_timer st.iflag; timer }
        else
          st
      in
      (* dma *)
      let st = DMA_OAM.exec_dma st mc in
      let st = DMA_VRAM.exec_dma st mc in
      (* ppu *)
      let st, render = PPU.process_ppu st @@ PPU.dot_of_mc mc @@ State.get_speed st in
      st, State.mc_to_time st mc, render
      (* w "mainie" bedizemy dodawac st ppu do listy debuggera, oraz wyswietlac kolejne piksele z ppu *)
    | Halted _ ->
      (* check for interrupt *)
      (* Utils.print_hex "Halted loop PC state" st.regs._PC; *)
      let st, mc = poll_interrupts_halted st in
      (* timer *)
      (* run div *)
      let st = { st with timer = IOregs.Timer.run_div st.timer mc } in
      (* run tima *)
      let timer, if_requested = IOregs.Timer.run_tima st.timer mc in
      let st =
        if if_requested then
          { st with iflag = IOregs.Interrupts.request_timer st.iflag; timer }
        else
          st
      in
      (* dma  *)
      let st = DMA_OAM.exec_dma st mc in
      (* hdma *)
      let st = DMA_VRAM.exec_dma st mc in
      (* ppu  *)
      let st, render = PPU.process_ppu st @@ PPU.dot_of_mc mc @@ State.get_speed st in
      st, State.mc_to_time st mc, render
    | Stopped ->
      if ((IOregs.Joypad.get st.joypad 0) land 0x0F) != 0x0F then
        { st with activity = Running }, 0., false
      else
        st, 0., false


  let init_gb rom =
    let initial_state = State.load_rom (State.init_cgb State.initial) rom in
    initial_state

  let print_registers = State.print_registers
  let print_interrupts = State.print_interrupts
  let print_palettes = State.print_palettes


end

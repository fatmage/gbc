
module type S = sig
  type state
  module State : State.S
  module PPU : PPU.S
  module Instruction : Instruction.S

  val cpu_step : state -> PPU.t -> state * PPU.t * float

  val init_gb : bytes -> state * PPU.t

end

module Make (State : State.S) : (S with type state = State.t) = struct
  type state = State.t
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

  let fetch_decode st : Instruction.instruction =
    match State.Bus.get8 st st.regs._PC with
    | 0x00 -> Instruction.iNOP
    | 0x01 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 BC n16
    | 0x02 -> Instruction.iLD_r16pA BC
    | 0x03 -> Instruction.iINC_r16 BC
    | 0x04 -> Instruction.iINC_r8 B
    | 0x05 -> Instruction.iDEC_r8 B
    | 0x06 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 B n8
    | 0x07 -> Instruction.iRLCA
    | 0x08 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_n16pSP n16
    | 0x09 -> Instruction.iADD_HLr16 BC
    | 0x0A -> Instruction.iLD_Ar16p BC
    | 0x0B -> Instruction.iDEC_r16 BC
    | 0x0C -> Instruction.iINC_r8 C
    | 0x0D -> Instruction.iDEC_r8 C
    | 0x0E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 C n8
    | 0x0F -> Instruction.iRRCA
    | 0x10 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iSTOP n8
    | 0x11 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 DE n16
    | 0x12 -> Instruction.iLD_r16pA DE
    | 0x13 -> Instruction.iINC_r16 DE
    | 0x14 -> Instruction.iINC_r8 D
    | 0x15 -> Instruction.iDEC_r8 D
    | 0x16 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 D n8
    | 0x17 -> Instruction.iRLA
    | 0x18 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_n8 n8
    | 0x19 -> Instruction.iADD_HLr16 DE
    | 0x1A -> Instruction.iLD_Ar16p DE
    | 0x1B -> Instruction.iDEC_r16 DE
    | 0x1C -> Instruction.iINC_r8 E
    | 0x1D -> Instruction.iDEC_r8 E
    | 0x1E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 E n8
    | 0x1F -> Instruction.iRRA
    | 0x20 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cnz n8
    | 0x21 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_rn16 HL n16
    | 0x22 -> Instruction.iLD_HLIpA
    | 0x23 -> Instruction.iINC_r16 HL
    | 0x24 -> Instruction.iINC_r8 H
    | 0x25 -> Instruction.iDEC_r8 H
    | 0x26 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 H n8
    | 0x27 -> Instruction.iDAA
    | 0x28 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cz n8
    | 0x29 -> Instruction.iADD_HLr16 HL
    | 0x2A -> Instruction.iLD_AHLIp
    | 0x2B -> Instruction.iDEC_r16 HL
    | 0x2C -> Instruction.iINC_r8 L
    | 0x2D -> Instruction.iDEC_r8 L
    | 0x2E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 L n8
    | 0x2F -> Instruction.iCPL
    | 0x30 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cnc n8
    | 0x31 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_SPn16 n16
    | 0x32 -> Instruction.iLD_HLDpA
    | 0x33 -> Instruction.iINC_SP
    | 0x34 -> Instruction.iINC_HLp
    | 0x35 -> Instruction.iDEC_HLp
    | 0x36 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_HLpn8 n8
    | 0x37 -> Instruction.iSCF
    | 0x38 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iJR_cn8 Cc n8
    | 0x39 -> Instruction.iADD_HLSP
    | 0x3A -> Instruction.iLD_AHLDp
    | 0x3B -> Instruction.iDEC_SP
    | 0x3C -> Instruction.iINC_r8 A
    | 0x3D -> Instruction.iDEC_r8 A
    | 0x3E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_rn8 A n8
    | 0x3F -> Instruction.iCCF
    | 0x40 -> Instruction.iLD_rr8 B B
    | 0x41 -> Instruction.iLD_rr8 B C
    | 0x42 -> Instruction.iLD_rr8 B D
    | 0x43 -> Instruction.iLD_rr8 B E
    | 0x44 -> Instruction.iLD_rr8 B H
    | 0x45 -> Instruction.iLD_rr8 B L
    | 0x46 -> Instruction.iLD_r8HLp B
    | 0x47 -> Instruction.iLD_rr8 B A
    | 0x48 -> Instruction.iLD_rr8 C B
    | 0x49 -> Instruction.iLD_rr8 C C
    | 0x4A -> Instruction.iLD_rr8 C D
    | 0x4B -> Instruction.iLD_rr8 C E
    | 0x4C -> Instruction.iLD_rr8 C H
    | 0x4D -> Instruction.iLD_rr8 C L
    | 0x4E -> Instruction.iLD_r8HLp C
    | 0x4F -> Instruction.iLD_rr8 C A
    | 0x50 -> Instruction.iLD_rr8 D B
    | 0x51 -> Instruction.iLD_rr8 D C
    | 0x52 -> Instruction.iLD_rr8 D D
    | 0x53 -> Instruction.iLD_rr8 D E
    | 0x54 -> Instruction.iLD_rr8 D H
    | 0x55 -> Instruction.iLD_rr8 D L
    | 0x56 -> Instruction.iLD_r8HLp D
    | 0x57 -> Instruction.iLD_rr8 D A
    | 0x58 -> Instruction.iLD_rr8 E B
    | 0x59 -> Instruction.iLD_rr8 E C
    | 0x5A -> Instruction.iLD_rr8 E D
    | 0x5B -> Instruction.iLD_rr8 E E
    | 0x5C -> Instruction.iLD_rr8 E H
    | 0x5D -> Instruction.iLD_rr8 E L
    | 0x5E -> Instruction.iLD_r8HLp E
    | 0x5F -> Instruction.iLD_rr8 E A
    | 0x60 -> Instruction.iLD_rr8 H B
    | 0x61 -> Instruction.iLD_rr8 H C
    | 0x62 -> Instruction.iLD_rr8 H D
    | 0x63 -> Instruction.iLD_rr8 H E
    | 0x64 -> Instruction.iLD_rr8 H H
    | 0x65 -> Instruction.iLD_rr8 H L
    | 0x66 -> Instruction.iLD_r8HLp H
    | 0x67 -> Instruction.iLD_rr8 H A
    | 0x68 -> Instruction.iLD_rr8 L B
    | 0x69 -> Instruction.iLD_rr8 L C
    | 0x6A -> Instruction.iLD_rr8 L D
    | 0x6B -> Instruction.iLD_rr8 L E
    | 0x6C -> Instruction.iLD_rr8 L H
    | 0x6D -> Instruction.iLD_rr8 L L
    | 0x6E -> Instruction.iLD_r8HLp L
    | 0x6F -> Instruction.iLD_rr8 L A
    | 0x70 -> Instruction.iLD_HLpr8 B
    | 0x71 -> Instruction.iLD_HLpr8 C
    | 0x72 -> Instruction.iLD_HLpr8 D
    | 0x73 -> Instruction.iLD_HLpr8 E
    | 0x74 -> Instruction.iLD_HLpr8 H
    | 0x75 -> Instruction.iLD_HLpr8 L
    | 0x76 -> Instruction.iHALT
    | 0x77 -> Instruction.iLD_HLpr8 A
    | 0x78 -> Instruction.iLD_rr8 A B
    | 0x79 -> Instruction.iLD_rr8 A C
    | 0x7A -> Instruction.iLD_rr8 A D
    | 0x7B -> Instruction.iLD_rr8 A E
    | 0x7C -> Instruction.iLD_rr8 A H
    | 0x7D -> Instruction.iLD_rr8 A L
    | 0x7E -> Instruction.iLD_r8HLp A
    | 0x7F -> Instruction.iLD_rr8 A A
    | 0x80 -> Instruction.iADD_Ar8 B
    | 0x81 -> Instruction.iADD_Ar8 C
    | 0x82 -> Instruction.iADD_Ar8 D
    | 0x83 -> Instruction.iADD_Ar8 E
    | 0x84 -> Instruction.iADD_Ar8 H
    | 0x85 -> Instruction.iADD_Ar8 L
    | 0x86 -> Instruction.iADD_AHLp
    | 0x87 -> Instruction.iADD_Ar8 A
    | 0x88 -> Instruction.iADC_Ar8 B
    | 0x89 -> Instruction.iADC_Ar8 C
    | 0x8A -> Instruction.iADC_Ar8 D
    | 0x8B -> Instruction.iADC_Ar8 E
    | 0x8C -> Instruction.iADC_Ar8 H
    | 0x8D -> Instruction.iADC_Ar8 L
    | 0x8E -> Instruction.iADC_AHLp
    | 0x8F -> Instruction.iADC_Ar8 A
    | 0x90 -> Instruction.iSUB_Ar8 B
    | 0x91 -> Instruction.iSUB_Ar8 C
    | 0x92 -> Instruction.iSUB_Ar8 D
    | 0x93 -> Instruction.iSUB_Ar8 E
    | 0x94 -> Instruction.iSUB_Ar8 H
    | 0x95 -> Instruction.iSUB_Ar8 L
    | 0x96 -> Instruction.iSUB_AHLp
    | 0x97 -> Instruction.iSUB_Ar8 A
    | 0x98 -> Instruction.iSBC_Ar8 B
    | 0x99 -> Instruction.iSBC_Ar8 C
    | 0x9A -> Instruction.iSBC_Ar8 D
    | 0x9B -> Instruction.iSBC_Ar8 E
    | 0x9C -> Instruction.iSBC_Ar8 H
    | 0x9D -> Instruction.iSBC_Ar8 L
    | 0x9E -> Instruction.iSBC_AHLp
    | 0x9F -> Instruction.iSBC_Ar8 A
    | 0xA0 -> Instruction.iAND_Ar8 B
    | 0xA1 -> Instruction.iAND_Ar8 C
    | 0xA2 -> Instruction.iAND_Ar8 D
    | 0xA3 -> Instruction.iAND_Ar8 E
    | 0xA4 -> Instruction.iAND_Ar8 H
    | 0xA5 -> Instruction.iAND_Ar8 L
    | 0xA6 -> Instruction.iAND_AHLp
    | 0xA7 -> Instruction.iAND_Ar8 A
    | 0xA8 -> Instruction.iXOR_Ar8 B
    | 0xA9 -> Instruction.iXOR_Ar8 C
    | 0xAA -> Instruction.iXOR_Ar8 D
    | 0xAB -> Instruction.iXOR_Ar8 E
    | 0xAC -> Instruction.iXOR_Ar8 H
    | 0xAD -> Instruction.iXOR_Ar8 L
    | 0xAE -> Instruction.iXOR_AHLp
    | 0xAF -> Instruction.iXOR_Ar8 A
    | 0xB0 -> Instruction.iOR_Ar8 B
    | 0xB1 -> Instruction.iOR_Ar8 C
    | 0xB2 -> Instruction.iOR_Ar8 D
    | 0xB3 -> Instruction.iOR_Ar8 E
    | 0xB4 -> Instruction.iOR_Ar8 H
    | 0xB5 -> Instruction.iOR_Ar8 L
    | 0xB6 -> Instruction.iOR_AHLp
    | 0xB7 -> Instruction.iOR_Ar8 A
    | 0xB8 -> Instruction.iCP_Ar8 B
    | 0xB9 -> Instruction.iCP_Ar8 C
    | 0xBA -> Instruction.iCP_Ar8 D
    | 0xBB -> Instruction.iCP_Ar8 E
    | 0xBC -> Instruction.iCP_Ar8 H
    | 0xBD -> Instruction.iCP_Ar8 L
    | 0xBE -> Instruction.iCP_AHLp
    | 0xBF -> Instruction.iCP_Ar8 A
    | 0xC0 -> Instruction.iRET_c Cnz
    | 0xC1 -> Instruction.iPOP_r16 BC
    | 0xC2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cnz n16
    | 0xC3 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_n16 n16
    | 0xC4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cnz n16
    | 0xC5 -> Instruction.iPUSH_r16 BC
    | 0xC6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADD_An8 n8
    | 0xC7 -> Instruction.iRST_vec 0x00
    | 0xC8 -> Instruction.iRET_c Cz
    | 0xC9 -> Instruction.iRET
    | 0xCA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cz n16
    (* | 0xCB ->  prefix CB *)
    | 0xCC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cz n16
    | 0xCD -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_n16 n16
    | 0xCE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADC_An8 n8
    | 0xCF -> Instruction.iRST_vec 0x08
    | 0xD0 -> Instruction.iRET_c Cnc
    | 0xD1 -> Instruction.iPOP_r16 DE
    | 0xD2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cnc n16
    (* | 0xD3 ->  iNOP*)
    | 0xD4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cnc n16
    | 0xD5 -> Instruction.iPUSH_r16 DE
    | 0xD6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iSUB_An8 n8
    | 0xD7 -> Instruction.iRST_vec 0x10
    | 0xD8 -> Instruction.iRET_c Cc
    | 0xD9 -> Instruction.iRETI
    | 0xDA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iJP_cn16 Cc n16
    (* | 0xDB -> Instruction.iNOP  *)
    | 0xDC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iCALL_cn16 Cc n16
    (* | 0xDD -> Instruction.iNOP  *)
    | 0xDE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iSBC_An8 n8
    | 0xDF -> Instruction.iRST_vec 0x18
    | 0xE0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLDH_n16pA n8
    | 0xE1 -> Instruction.iPOP_r16 HL
    | 0xE2 -> Instruction.iLDH_CpA
    (* | 0xE3 -> Instruction.iNOP  *)
    (* | 0xE4 -> Instruction.iNOP  *)
    | 0xE5 -> Instruction.iPUSH_r16 HL
    | 0xE6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iAND_An8 n8
    | 0xE7 -> Instruction.iRST_vec 0x20
    | 0xE8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iADD_SPe8 n8
    | 0xE9 -> Instruction.iJP_HL
    | 0xEA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_n16pA n16
    (* | 0xEB -> Instruction.iNOP  *)
    (* | 0xEC -> Instruction.iNOP  *)
    (* | 0xED -> Instruction.iNOP  *)
    | 0xEE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iXOR_An8 n8
    | 0xEF -> Instruction.iRST_vec 0x28
    | 0xF0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLDH_An16p n8
    | 0xF1 -> Instruction.iPOP_AF
    | 0xF2 -> Instruction.iLDH_ACp
    | 0xF3 -> Instruction.iDI
    (* | 0xF4 -> Instruction.iNOP  *)
    | 0xF5 -> Instruction.iPUSH_AF
    | 0xF6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iOR_An8 n8
    | 0xF7 -> Instruction.iRST_vec 0x30
    | 0xF8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iLD_HLSPe8 n8
    | 0xF9 -> Instruction.iLD_SPHL
    | 0xFA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in Instruction.iLD_An16p n16
    | 0xFB -> Instruction.iEI
    (* | 0xFC -> Instruction.iNOP  *)
    (* | 0xFD -> Instruction.iNOP  *)
    | 0xFE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in Instruction.iCP_An8 n8
    | 0xFF -> Instruction.iRST_vec 0x38
    (* CB-prefixed *)
    | 0xCB -> match_prefixed st
    | _    -> Instruction.iNOP

  let first_set_bit =
    let rec aux n m k =
      match n land m with
      | 0 -> aux n (m * 2) (k + 1)
      | _ -> k
    in
    function
    | 0 -> 0
    | n -> aux n 1 1

  let fetch_decode_execute (st : state) =
    let st, instr =
      match st.ime with
      | Enabled  ->
        let addr =
          match IOregs.IE.get st.ie 0xFFFF land IOregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
          | 1 -> 0x40 (* VBlank *)
          | 2 -> 0x48 (* LCD *)
          | 3 -> 0x50 (* Timer *)
          | 4 -> 0x58 (* Serial *)
          | 5 -> 0x60 (* Joypad *)
          | _ -> 0x00 (* None *)
        in
        begin match addr with
        | 0 -> st, fetch_decode st
        | n -> st, Instruction.interrupt_service_routine n
        end
      | Enabling -> { st with ime = Enabled }, fetch_decode st
      | Disabled -> st, fetch_decode st
    in
    match instr st with
    | st, Stop, cycles -> st, cycles
    | st, Halt, cycles -> st, cycles
    | st, Jump, cycles -> st, cycles
    | st, Next, cycles ->
      { st with regs = { st.regs with _PC = st.regs._PC + cycles } }, cycles

  let poll_interrupts_halted (st : state) =
    let st, addr, (act : State.cpu_activity) =
      match st.ime with
      | Enabled ->
          begin match IOregs.IE.get st.ie 0xFFFF land IOregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
          | 1 -> { st with activity = Running }, 0x40, Running
          | 2 -> { st with activity = Running }, 0x48, Running
          | 3 -> { st with activity = Running }, 0x50, Running
          | 4 -> { st with activity = Running }, 0x58, Running
          | 5 -> { st with activity = Running }, 0x60, Running
          | _ -> st, 0x00, Halted
          end
      | Disabled ->
          begin match IOregs.IE.get st.ie 0xFFFF land IOregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
          | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
          | _ -> st, 0x00, Halted
          end
      | Enabling ->
          begin match IOregs.IE.get st.ie 0xFFFF land IOregs.Interrupts.get st.iflag 0xFF0F |> first_set_bit with
          | 1 | 2 | 3 | 4 | 5 -> { st with activity = Running }, 0x00, Running
          | _ -> st, 0x00, Halted
          end
    in
    match act, addr with
    | Running, 0x00 -> fetch_decode st st
    | Running, n    -> Instruction.interrupt_service_routine n st
    | Halted,  _    -> st, Halt, 1

  let cpu_step (st : state) ppu =
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
      let st, ppu = PPU.process_ppu st ppu @@ PPU.dot_of_mc mc @@ State.get_speed st in
      st, ppu, State.mc_to_time st mc
      (* w "mainie" bedizemy dodawac st ppu do listy debuggera, oraz wyswietlac kolejne piksele z ppu *)
    | Halted ->
      (* check for interrupt *)
      let st, _, mc = poll_interrupts_halted st in
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
      let st, ppu = PPU.process_ppu st ppu @@ PPU.dot_of_mc mc @@ State.get_speed st in
      st, ppu, State.mc_to_time st mc
    | Stopped n ->
      let mc = 4 in

      (* idea - do a set amount of cycles, progress dma hdma and ppu, and then after reaching x cycles change to running *)
      st, ppu, State.mc_to_time st mc


  let init_gb rom =
    let initial_state = State.load_rom (State.init_cgb State.initial) rom in
    initial_state, PPU.initial


end

open Instruction

let match_prefixed st =
  match State.Bus.get8 st (st.regs._PC + 1) with
  | 0x00 -> iRLC_r8 B
  | 0x01 -> iRLC_r8 C
  | 0x02 -> iRLC_r8 D
  | 0x03 -> iRLC_r8 E
  | 0x04 -> iRLC_r8 H
  | 0x05 -> iRLC_r8 L
  | 0x06 -> iRLC_HLp
  | 0x07 -> iRLC_r8 A
  | 0x08 -> iRRC_r8 B
  | 0x09 -> iRRC_r8 C
  | 0x0A -> iRRC_r8 D
  | 0x0B -> iRRC_r8 E
  | 0x0C -> iRRC_r8 H
  | 0x0D -> iRRC_r8 L
  | 0x0E -> iRRC_HLp
  | 0x0F -> iRRC_r8 A
  | 0x10 -> iRL_r8 B
  | 0x11 -> iRL_r8 C
  | 0x12 -> iRL_r8 D
  | 0x13 -> iRL_r8 E
  | 0x14 -> iRL_r8 H
  | 0x15 -> iRL_r8 L
  | 0x16 -> iRL_HLp
  | 0x17 -> iRL_r8 A
  | 0x18 -> iRR_r8 B
  | 0x19 -> iRR_r8 C
  | 0x1A -> iRR_r8 D
  | 0x1B -> iRR_r8 E
  | 0x1C -> iRR_r8 H
  | 0x1D -> iRR_r8 L
  | 0x1E -> iRR_HLp
  | 0x1F -> iRR_r8 A
  | 0x20 -> iSLA_r8 B
  | 0x21 -> iSLA_r8 C
  | 0x22 -> iSLA_r8 D
  | 0x23 -> iSLA_r8 E
  | 0x24 -> iSLA_r8 H
  | 0x25 -> iSLA_r8 L
  | 0x26 -> iSLA_HLp
  | 0x27 -> iSLA_r8 A
  | 0x28 -> iSRA_r8 B
  | 0x29 -> iSRA_r8 C
  | 0x2A -> iSRA_r8 D
  | 0x2B -> iSRA_r8 E
  | 0x2C -> iSRA_r8 H
  | 0x2D -> iSRA_r8 L
  | 0x2E -> iSRA_HLp
  | 0x2F -> iSRA_r8 A
  | 0x30 -> iSWAP_r8 B
  | 0x31 -> iSWAP_r8 C
  | 0x32 -> iSWAP_r8 D
  | 0x33 -> iSWAP_r8 E
  | 0x34 -> iSWAP_r8 H
  | 0x35 -> iSWAP_r8 L
  | 0x36 -> iSWAP_HLp
  | 0x37 -> iSWAP_r8 A
  | 0x38 -> iSRL_r8 B
  | 0x39 -> iSRL_r8 C
  | 0x3A -> iSRL_r8 D
  | 0x3B -> iSRL_r8 E
  | 0x3C -> iSRL_r8 H
  | 0x3D -> iSRL_r8 L
  | 0x3E -> iSRL_HLp
  | 0x3F -> iSRL_r8 A
  | 0x40 -> iBIT_u3r8 0 B
  | 0x41 -> iBIT_u3r8 0 C
  | 0x42 -> iBIT_u3r8 0 D
  | 0x43 -> iBIT_u3r8 0 E
  | 0x44 -> iBIT_u3r8 0 H
  | 0x45 -> iBIT_u3r8 0 L
  | 0x46 -> iBIT_u3HLp 0
  | 0x47 -> iBIT_u3r8 0 A
  | 0x48 -> iBIT_u3r8 1 B
  | 0x49 -> iBIT_u3r8 1 C
  | 0x4A -> iBIT_u3r8 1 D
  | 0x4B -> iBIT_u3r8 1 E
  | 0x4C -> iBIT_u3r8 1 H
  | 0x4D -> iBIT_u3r8 1 L
  | 0x4E -> iBIT_u3HLp 1
  | 0x4F -> iBIT_u3r8 1 A
  | 0x50 -> iBIT_u3r8 2 B
  | 0x51 -> iBIT_u3r8 2 C
  | 0x52 -> iBIT_u3r8 2 D
  | 0x53 -> iBIT_u3r8 2 E
  | 0x54 -> iBIT_u3r8 2 H
  | 0x55 -> iBIT_u3r8 2 L
  | 0x56 -> iBIT_u3HLp 2
  | 0x57 -> iBIT_u3r8 2 A
  | 0x58 -> iBIT_u3r8 3 B
  | 0x59 -> iBIT_u3r8 3 C
  | 0x5A -> iBIT_u3r8 3 D
  | 0x5B -> iBIT_u3r8 3 E
  | 0x5C -> iBIT_u3r8 3 H
  | 0x5D -> iBIT_u3r8 3 L
  | 0x5E -> iBIT_u3HLp 3
  | 0x5F -> iBIT_u3r8 3 A
  | 0x60 -> iBIT_u3r8 4 B
  | 0x61 -> iBIT_u3r8 4 C
  | 0x62 -> iBIT_u3r8 4 D
  | 0x63 -> iBIT_u3r8 4 E
  | 0x64 -> iBIT_u3r8 4 H
  | 0x65 -> iBIT_u3r8 4 L
  | 0x66 -> iBIT_u3HLp 4
  | 0x67 -> iBIT_u3r8 4 A
  | 0x68 -> iBIT_u3r8 5 B
  | 0x69 -> iBIT_u3r8 5 C
  | 0x6A -> iBIT_u3r8 5 D
  | 0x6B -> iBIT_u3r8 5 E
  | 0x6C -> iBIT_u3r8 5 H
  | 0x6D -> iBIT_u3r8 5 L
  | 0x6E -> iBIT_u3HLp 5
  | 0x6F -> iBIT_u3r8 5 A
  | 0x70 -> iBIT_u3r8 6 B
  | 0x71 -> iBIT_u3r8 6 C
  | 0x72 -> iBIT_u3r8 6 D
  | 0x73 -> iBIT_u3r8 6 E
  | 0x74 -> iBIT_u3r8 6 H
  | 0x75 -> iBIT_u3r8 6 L
  | 0x76 -> iBIT_u3HLp 6
  | 0x77 -> iBIT_u3r8 6 A
  | 0x78 -> iBIT_u3r8 7 B
  | 0x79 -> iBIT_u3r8 7 C
  | 0x7A -> iBIT_u3r8 7 D
  | 0x7B -> iBIT_u3r8 7 E
  | 0x7C -> iBIT_u3r8 7 H
  | 0x7D -> iBIT_u3r8 7 L
  | 0x7E -> iBIT_u3HLp 7
  | 0x7F -> iBIT_u3r8 7 A
  | 0x80 -> iRES_u3r8 0 B
  | 0x81 -> iRES_u3r8 0 C
  | 0x82 -> iRES_u3r8 0 D
  | 0x83 -> iRES_u3r8 0 E
  | 0x84 -> iRES_u3r8 0 H
  | 0x85 -> iRES_u3r8 0 L
  | 0x86 -> iRES_u3HLp 0
  | 0x87 -> iRES_u3r8 0 A
  | 0x88 -> iRES_u3r8 1 B
  | 0x89 -> iRES_u3r8 1 C
  | 0x8A -> iRES_u3r8 1 D
  | 0x8B -> iRES_u3r8 1 E
  | 0x8C -> iRES_u3r8 1 H
  | 0x8D -> iRES_u3r8 1 L
  | 0x8E -> iRES_u3HLp 1
  | 0x8F -> iRES_u3r8 1 A
  | 0x90 -> iRES_u3r8 2 B
  | 0x91 -> iRES_u3r8 2 C
  | 0x92 -> iRES_u3r8 2 D
  | 0x93 -> iRES_u3r8 2 E
  | 0x94 -> iRES_u3r8 2 H
  | 0x95 -> iRES_u3r8 2 L
  | 0x96 -> iRES_u3HLp 2
  | 0x97 -> iRES_u3r8 2 A
  | 0x98 -> iRES_u3r8 3 B
  | 0x99 -> iRES_u3r8 3 C
  | 0x9A -> iRES_u3r8 3 D
  | 0x9B -> iRES_u3r8 3 E
  | 0x9C -> iRES_u3r8 3 H
  | 0x9D -> iRES_u3r8 3 L
  | 0x9E -> iRES_u3HLp 3
  | 0x9F -> iRES_u3r8 3 A
  | 0xA0 -> iRES_u3r8 4 B
  | 0xA1 -> iRES_u3r8 4 C
  | 0xA2 -> iRES_u3r8 4 D
  | 0xA3 -> iRES_u3r8 4 E
  | 0xA4 -> iRES_u3r8 4 H
  | 0xA5 -> iRES_u3r8 4 L
  | 0xA6 -> iRES_u3HLp 4
  | 0xA7 -> iRES_u3r8 4 A
  | 0xA8 -> iRES_u3r8 5 B
  | 0xA9 -> iRES_u3r8 5 C
  | 0xAA -> iRES_u3r8 5 D
  | 0xAB -> iRES_u3r8 5 E
  | 0xAC -> iRES_u3r8 5 H
  | 0xAD -> iRES_u3r8 5 L
  | 0xAE -> iRES_u3HLp 5
  | 0xAF -> iRES_u3r8 5 A
  | 0xB0 -> iRES_u3r8 6 B
  | 0xB1 -> iRES_u3r8 6 C
  | 0xB2 -> iRES_u3r8 6 D
  | 0xB3 -> iRES_u3r8 6 E
  | 0xB4 -> iRES_u3r8 6 H
  | 0xB5 -> iRES_u3r8 6 L
  | 0xB6 -> iRES_u3HLp 6
  | 0xB7 -> iRES_u3r8 6 A
  | 0xB8 -> iRES_u3r8 7 B
  | 0xB9 -> iRES_u3r8 7 C
  | 0xBA -> iRES_u3r8 7 D
  | 0xBB -> iRES_u3r8 7 E
  | 0xBC -> iRES_u3r8 7 H
  | 0xBD -> iRES_u3r8 7 L
  | 0xBE -> iRES_u3HLp 7
  | 0xBF -> iRES_u3r8 7 A
  | 0xC0 -> iSET_u3r8 0 B
  | 0xC1 -> iSET_u3r8 0 C
  | 0xC2 -> iSET_u3r8 0 D
  | 0xC3 -> iSET_u3r8 0 E
  | 0xC4 -> iSET_u3r8 0 H
  | 0xC5 -> iSET_u3r8 0 L
  | 0xC6 -> iSET_u3HLp 0
  | 0xC7 -> iSET_u3r8 0 A
  | 0xC8 -> iSET_u3r8 1 B
  | 0xC9 -> iSET_u3r8 1 C
  | 0xCA -> iSET_u3r8 1 D
  | 0xCB -> iSET_u3r8 1 E
  | 0xCC -> iSET_u3r8 1 H
  | 0xCD -> iSET_u3r8 1 L
  | 0xCE -> iSET_u3HLp 1
  | 0xCF -> iSET_u3r8 1 A
  | 0xD0 -> iSET_u3r8 2 B
  | 0xD1 -> iSET_u3r8 2 C
  | 0xD2 -> iSET_u3r8 2 D
  | 0xD3 -> iSET_u3r8 2 E
  | 0xD4 -> iSET_u3r8 2 H
  | 0xD5 -> iSET_u3r8 2 L
  | 0xD6 -> iSET_u3HLp 2
  | 0xD7 -> iSET_u3r8 2 A
  | 0xD8 -> iSET_u3r8 3 B
  | 0xD9 -> iSET_u3r8 3 C
  | 0xDA -> iSET_u3r8 3 D
  | 0xDB -> iSET_u3r8 3 E
  | 0xDC -> iSET_u3r8 3 H
  | 0xDD -> iSET_u3r8 3 L
  | 0xDE -> iSET_u3HLp 3
  | 0xDF -> iSET_u3r8 3 A
  | 0xE0 -> iSET_u3r8 4 B
  | 0xE1 -> iSET_u3r8 4 C
  | 0xE2 -> iSET_u3r8 4 D
  | 0xE3 -> iSET_u3r8 4 E
  | 0xE4 -> iSET_u3r8 4 H
  | 0xE5 -> iSET_u3r8 4 L
  | 0xE6 -> iSET_u3HLp 4
  | 0xE7 -> iSET_u3r8 4 A
  | 0xE8 -> iSET_u3r8 5 B
  | 0xE9 -> iSET_u3r8 5 C
  | 0xEA -> iSET_u3r8 5 D
  | 0xEB -> iSET_u3r8 5 E
  | 0xEC -> iSET_u3r8 5 H
  | 0xED -> iSET_u3r8 5 L
  | 0xEE -> iSET_u3HLp 5
  | 0xEF -> iSET_u3r8 5 A
  | 0xF0 -> iSET_u3r8 6 B
  | 0xF1 -> iSET_u3r8 6 C
  | 0xF2 -> iSET_u3r8 6 D
  | 0xF3 -> iSET_u3r8 6 E
  | 0xF4 -> iSET_u3r8 6 H
  | 0xF5 -> iSET_u3r8 6 L
  | 0xF6 -> iSET_u3HLp 6
  | 0xF7 -> iSET_u3r8 6 A
  | 0xF8 -> iSET_u3r8 7 B
  | 0xF9 -> iSET_u3r8 7 C
  | 0xFA -> iSET_u3r8 7 D
  | 0xFB -> iSET_u3r8 7 E
  | 0xFC -> iSET_u3r8 7 H
  | 0xFD -> iSET_u3r8 7 L
  | 0xFE -> iSET_u3HLp 7
  | 0xFF -> iSET_u3r8 7 A
  | _ -> iNOP

let fetch_decode st : instruction =
  match State.Bus.get8 st st.regs._PC with
  | 0x00 -> iNOP
  | 0x01 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_rn16 BC n16
  | 0x02 -> iLD_r16pA BC
  | 0x03 -> iINC_r16 BC
  | 0x04 -> iINC_r8 B
  | 0x05 -> iDEC_r8 B
  | 0x06 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 B n8
  | 0x07 -> iRLCA
  | 0x08 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_n16pSP n16
  | 0x09 -> iADD_HLr16 BC
  | 0x0A -> iLD_Ar16p BC
  | 0x0B -> iDEC_r16 BC
  | 0x0C -> iINC_r8 C
  | 0x0D -> iDEC_r8 C
  | 0x0E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 C n8
  | 0x0F -> iRRCA
  | 0x10 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iSTOP n8
  | 0x11 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_rn16 DE n16
  | 0x12 -> iLD_r16pA DE
  | 0x13 -> iINC_r16 DE
  | 0x14 -> iINC_r8 D
  | 0x15 -> iDEC_r8 D
  | 0x16 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 D n8
  | 0x17 -> iRLA
  | 0x18 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iJR_n8 n8
  | 0x19 -> iADD_HLr16 DE
  | 0x1A -> iLD_Ar16p DE
  | 0x1B -> iDEC_r16 DE
  | 0x1C -> iINC_r8 E
  | 0x1D -> iDEC_r8 E
  | 0x1E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 E n8
  | 0x1F -> iRRA
  | 0x20 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iJR_cn8 Cnz n8
  | 0x21 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_rn16 HL n16
  | 0x22 -> iLD_HLIpA
  | 0x23 -> iINC_r16 HL
  | 0x24 -> iINC_r8 H
  | 0x25 -> iDEC_r8 H
  | 0x26 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 H n8
  | 0x27 -> iDAA
  | 0x28 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iJR_cn8 Cz n8
  | 0x29 -> iADD_HLr16 HL
  | 0x2A -> iLD_AHLIp
  | 0x2B -> iDEC_r16 HL
  | 0x2C -> iINC_r8 L
  | 0x2D -> iDEC_r8 L
  | 0x2E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 L n8
  | 0x2F -> iCPL
  | 0x30 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iJR_cn8 Cnc n8
  | 0x31 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_SPn16 n16
  | 0x32 -> iLD_HLDpA
  | 0x33 -> iINC_SP
  | 0x34 -> iINC_HLp
  | 0x35 -> iDEC_HLp
  | 0x36 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_HLpn8 n8
  | 0x37 -> iSCF
  | 0x38 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iJR_cn8 Cc n8
  | 0x39 -> iADD_HLSP
  | 0x3A -> iLD_AHLDp
  | 0x3B -> iDEC_SP
  | 0x3C -> iINC_r8 A
  | 0x3D -> iDEC_r8 A
  | 0x3E -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_rn8 A n8
  | 0x3F -> iCCF
  | 0x40 -> iLD_rr8 B B
  | 0x41 -> iLD_rr8 B C
  | 0x42 -> iLD_rr8 B D
  | 0x43 -> iLD_rr8 B E
  | 0x44 -> iLD_rr8 B H
  | 0x45 -> iLD_rr8 B L
  | 0x46 -> iLD_r8HLp B
  | 0x47 -> iLD_rr8 B A
  | 0x48 -> iLD_rr8 C B
  | 0x49 -> iLD_rr8 C C
  | 0x4A -> iLD_rr8 C D
  | 0x4B -> iLD_rr8 C E
  | 0x4C -> iLD_rr8 C H
  | 0x4D -> iLD_rr8 C L
  | 0x4E -> iLD_r8HLp C
  | 0x4F -> iLD_rr8 C A
  | 0x50 -> iLD_rr8 D B
  | 0x51 -> iLD_rr8 D C
  | 0x52 -> iLD_rr8 D D
  | 0x53 -> iLD_rr8 D E
  | 0x54 -> iLD_rr8 D H
  | 0x55 -> iLD_rr8 D L
  | 0x56 -> iLD_r8HLp D
  | 0x57 -> iLD_rr8 D A
  | 0x58 -> iLD_rr8 E B
  | 0x59 -> iLD_rr8 E C
  | 0x5A -> iLD_rr8 E D
  | 0x5B -> iLD_rr8 E E
  | 0x5C -> iLD_rr8 E H
  | 0x5D -> iLD_rr8 E L
  | 0x5E -> iLD_r8HLp E
  | 0x5F -> iLD_rr8 E A
  | 0x60 -> iLD_rr8 H B
  | 0x61 -> iLD_rr8 H C
  | 0x62 -> iLD_rr8 H D
  | 0x63 -> iLD_rr8 H E
  | 0x64 -> iLD_rr8 H H
  | 0x65 -> iLD_rr8 H L
  | 0x66 -> iLD_r8HLp H
  | 0x67 -> iLD_rr8 H A
  | 0x68 -> iLD_rr8 L B
  | 0x69 -> iLD_rr8 L C
  | 0x6A -> iLD_rr8 L D
  | 0x6B -> iLD_rr8 L E
  | 0x6C -> iLD_rr8 L H
  | 0x6D -> iLD_rr8 L L
  | 0x6E -> iLD_r8HLp L
  | 0x6F -> iLD_rr8 L A
  | 0x70 -> iLD_HLpr8 B
  | 0x71 -> iLD_HLpr8 C
  | 0x72 -> iLD_HLpr8 D
  | 0x73 -> iLD_HLpr8 E
  | 0x74 -> iLD_HLpr8 H
  | 0x75 -> iLD_HLpr8 L
  | 0x76 -> iHALT
  | 0x77 -> iLD_HLpr8 A
  | 0x78 -> iLD_rr8 A B
  | 0x79 -> iLD_rr8 A C
  | 0x7A -> iLD_rr8 A D
  | 0x7B -> iLD_rr8 A E
  | 0x7C -> iLD_rr8 A H
  | 0x7D -> iLD_rr8 A L
  | 0x7E -> iLD_r8HLp A
  | 0x7F -> iLD_rr8 A A
  | 0x80 -> iADD_Ar8 B
  | 0x81 -> iADD_Ar8 C
  | 0x82 -> iADD_Ar8 D
  | 0x83 -> iADD_Ar8 E
  | 0x84 -> iADD_Ar8 H
  | 0x85 -> iADD_Ar8 L
  | 0x86 -> iADD_AHLp
  | 0x87 -> iADD_Ar8 A
  | 0x88 -> iADC_Ar8 B
  | 0x89 -> iADC_Ar8 C
  | 0x8A -> iADC_Ar8 D
  | 0x8B -> iADC_Ar8 E
  | 0x8C -> iADC_Ar8 H
  | 0x8D -> iADC_Ar8 L
  | 0x8E -> iADC_AHLp
  | 0x8F -> iADC_Ar8 A
  | 0x90 -> iSUB_Ar8 B
  | 0x91 -> iSUB_Ar8 C
  | 0x92 -> iSUB_Ar8 D
  | 0x93 -> iSUB_Ar8 E
  | 0x94 -> iSUB_Ar8 H
  | 0x95 -> iSUB_Ar8 L
  | 0x96 -> iSUB_AHLp
  | 0x97 -> iSUB_Ar8 A
  | 0x98 -> iSBC_Ar8 B
  | 0x99 -> iSBC_Ar8 C
  | 0x9A -> iSBC_Ar8 D
  | 0x9B -> iSBC_Ar8 E
  | 0x9C -> iSBC_Ar8 H
  | 0x9D -> iSBC_Ar8 L
  | 0x9E -> iSBC_AHLp
  | 0x9F -> iSBC_Ar8 A
  | 0xA0 -> iAND_Ar8 B
  | 0xA1 -> iAND_Ar8 C
  | 0xA2 -> iAND_Ar8 D
  | 0xA3 -> iAND_Ar8 E
  | 0xA4 -> iAND_Ar8 H
  | 0xA5 -> iAND_Ar8 L
  | 0xA6 -> iAND_AHLp
  | 0xA7 -> iAND_Ar8 A
  | 0xA8 -> iXOR_Ar8 B
  | 0xA9 -> iXOR_Ar8 C
  | 0xAA -> iXOR_Ar8 D
  | 0xAB -> iXOR_Ar8 E
  | 0xAC -> iXOR_Ar8 H
  | 0xAD -> iXOR_Ar8 L
  | 0xAE -> iXOR_AHLp
  | 0xAF -> iXOR_Ar8 A
  | 0xB0 -> iOR_Ar8 B
  | 0xB1 -> iOR_Ar8 C
  | 0xB2 -> iOR_Ar8 D
  | 0xB3 -> iOR_Ar8 E
  | 0xB4 -> iOR_Ar8 H
  | 0xB5 -> iOR_Ar8 L
  | 0xB6 -> iOR_AHLp
  | 0xB7 -> iOR_Ar8 A
  | 0xB8 -> iCP_Ar8 B
  | 0xB9 -> iCP_Ar8 C
  | 0xBA -> iCP_Ar8 D
  | 0xBB -> iCP_Ar8 E
  | 0xBC -> iCP_Ar8 H
  | 0xBD -> iCP_Ar8 L
  | 0xBE -> iCP_AHLp
  | 0xBF -> iCP_Ar8 A
  | 0xC0 -> iRET_c Cnz
  | 0xC1 -> iPOP_r16 BC
  | 0xC2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iJP_cn16 Cnz n16
  | 0xC3 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iJP_n16 n16
  | 0xC4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iCALL_cn16 Cnz n16
  | 0xC5 -> iPUSH_r16 BC
  | 0xC6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iADD_An8 n8
  | 0xC7 -> iRST_vec 0x00
  | 0xC8 -> iRET_c Cz
  | 0xC9 -> iRET
  | 0xCA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iJP_cn16 Cz n16
  (* | 0xCB ->  prefix CB *)
  | 0xCC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iCALL_cn16 Cz n16
  | 0xCD -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iCALL_n16 n16
  | 0xCE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iADC_An8 n8
  | 0xCF -> iRST_vec 0x08
  | 0xD0 -> iRET_c Cnc
  | 0xD1 -> iPOP_r16 DE
  | 0xD2 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iJP_cn16 Cnc n16
  (* | 0xD3 ->  iNOP*)
  | 0xD4 -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iCALL_cn16 Cnc n16
  | 0xD5 -> iPUSH_r16 DE
  | 0xD6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iSUB_An8 n8
  | 0xD7 -> iRST_vec 0x10
  | 0xD8 -> iRET_c Cc
  | 0xD9 -> iRETI
  | 0xDA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iJP_cn16 Cc n16
  (* | 0xDB -> iNOP  *)
  | 0xDC -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iCALL_cn16 Cc n16
  (* | 0xDD -> iNOP  *)
  | 0xDE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iSBC_An8 n8
  | 0xDF -> iRST_vec 0x18
  | 0xE0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLDH_n16pA n8
  | 0xE1 -> iPOP_r16 HL
  | 0xE2 -> iLDH_CpA
  (* | 0xE3 -> iNOP  *)
  (* | 0xE4 -> iNOP  *)
  | 0xE5 -> iPUSH_r16 HL
  | 0xE6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iAND_An8 n8
  | 0xE7 -> iRST_vec 0x20
  | 0xE8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iADD_SPe8 n8
  | 0xE9 -> iJP_HL
  | 0xEA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_n16pA n16
  (* | 0xEB -> iNOP  *)
  (* | 0xEC -> iNOP  *)
  (* | 0xED -> iNOP  *)
  | 0xEE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iXOR_An8 n8
  | 0xEF -> iRST_vec 0x28
  | 0xF0 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLDH_An16p n8
  | 0xF1 -> iPOP_AF
  | 0xF2 -> iLDH_ACp
  | 0xF3 -> iDI
  (* | 0xF4 -> iNOP  *)
  | 0xF5 -> iPUSH_AF
  | 0xF6 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iOR_An8 n8
  | 0xF7 -> iRST_vec 0x30
  | 0xF8 -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iLD_HLSPe8 n8
  | 0xF9 -> iLD_SPHL
  | 0xFA -> let n16 = State.Bus.get16 st (st.regs._PC + 1) in iLD_An16p n16
  | 0xFB -> iEI
  (* | 0xFC -> iNOP  *)
  (* | 0xFD -> iNOP  *)
  | 0xFE -> let n8 = State.Bus.get8 st (st.regs._PC + 1) in iCP_An8 n8
  | 0xFF -> iRST_vec 0x38
  (* CB-prefixed *)
  | 0xCB -> match_prefixed st
  | _    -> iNOP

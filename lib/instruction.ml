(* Instruction type *)

type instruction = State.t -> State.t * int

(* 8-bit Arithmetic and Logic Instructions *)

let iADC_Ar8 r8 = fun st ->
  let a, r = State.get_A st, State.get_r8 st r8 in
  let c = State.get_flag st Regs.Flag_c in
  let sum = a + r + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + r land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  1

let iADC_AHLp = fun st ->
  let a = State.get_A st in
  let hlp = State.get_HLp st in
  let c = State.get_flag st Regs.Flag_c in
  let sum = a + hlp + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + hlp land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  2

let iADC_An8 n = fun st ->
  let a, c = State.get_A st, State.get_flag st (Regs.Flag_c) in
  let sum = a + n + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + n land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  2

let iADD_Ar8 r8 = fun st ->
  let a, r = State.get_A st, State.get_r8 st r8 in
  let sum = a + r in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + r land 0xF > 0xF) ~c:(sum > 0xFF) (),
  1

let iADD_AHLp = fun st ->
  let a = State.get_A st in
  let hlp = State.get_HLp st in
  let sum = a + hlp in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + hlp land 0xF > 0xF) ~c:(sum > 0xFF) (),
  2

let iADD_An8 n = fun st ->
  let a = State.get_A st in
  let sum = a + n in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + n land 0xF > 0xF) ~c:(sum > 0xFF) (),
  2

let iAND_Ar8 r8 = fun st ->
  let result = State.get_A st land State.get_r8 st r8 in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true  ~c:false (),
  1

let iAND_AHLp = fun st ->
  let result = State.get_A st land State.get_HLp st in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true ~c:false (),
  2

let iAND_An8 n = fun st ->
  let result = State.get_A st land n in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true ~c:false (),
  2

let iCP_Ar8 r = fun st ->
  let a, y = State.get_A st, State.get_r8 st r in
  let result = a - y in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < y land 0xF) ~c:(y > a) (),
  1

let iCP_AHLp = fun st ->
  let a, hlp = State.get_A st, State.get_HLp st in
  let result = a - hlp in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF) ~c:(hlp > a) (),
  2

let iCP_An8 n = fun st ->
  let a = State.get_A st in
  let result = a - n in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < n land 0xF) ~c:(n > a) (),
  2

let iDEC_r8 r = fun st ->
  let x = State.get_r8 st r in
  let dec = x - 1 in let res = if dec < 0 then 0xFF else dec in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:true
  ~h:(x land 0x0F = 0x0) (),
  1

let iDEC_HLp = fun st ->
  let hlp = State.get_HLp st in
  let dec = hlp - 1 in let res = if dec < 0 then 0xFF else dec in
  State.set_flags (State.Bus.set8 st (State.get_HL st) res) ~z:(res = 0)
  ~n:true ~h:(hlp land 0x0F = 0x0) (),
  3

let iINC_r8 r = fun st ->
  let x = State.get_r8 st r in
  let inc = x + 1 in let res = if inc > 0xFF then 0 else inc in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:(x land 0x0F = 0x0F) (),
  1

let iINC_HLp = fun st ->
  let hlp = State.get_HLp st in
  let inc = hlp + 1 in let res = if inc > 0xFF then 0 else inc in
  State.set_flags (State.Bus.set8 st (State.get_HL st) res) ~z:(res = 0)
  ~n:false ~h:(hlp land 0x0F = 0x0F) (),
  3

let iOR_Ar8 r = fun st ->
  let res = State.get_A st lor State.get_r8 st r in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  1

let iOR_AHLp = fun st ->
  let res = State.get_A st lor State.get_HLp st in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  2

let iOR_An8 n = fun st ->
  let res = State.get_A st lor n in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  2

let iSBC_Ar8 r = fun st ->
  let a, x, c = State.get_A st, State.get_r8 st r, State.get_flag st Flag_c in
  let sub = a - x - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < x land 0xF + c) ~c:(sub < 0) (),
  1

let iSBC_AHLp = fun st ->
  let a, hlp, c = State.get_A st, State.get_HLp st, State.get_flag st Flag_c in
  let sub = a - hlp - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF + c) ~c:(sub < 0) (),
  2

let iSBC_An8 n = fun st ->
  let a, c = State.get_A st, State.get_flag st Flag_c in
  let sub = a - n - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < n land 0xF + c) ~c:(sub < 0) (),
  2

let iSUB_Ar8 r = fun st ->
  let a, x = State.get_A st, State.get_r8 st r in
  let sub = a - x in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < x land 0xF) ~c:(sub < 0) (),
  1

let iSUB_AHLp = fun st ->
  let a, hlp = State.get_A st, State.get_HLp st in
  let sub = a - hlp in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF) ~c:(sub < 0) (),
  2

let iSUB_An8 n = fun st ->
  let a = State.get_A st in
  let sub = a - n in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < n land 0xF) ~c:(sub < 0) () ,
  2

let iXOR_Ar8 r = fun st ->
  let res = State.get_A st lxor State.get_r8 st r in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  1

let iXOR_AHLp = fun st ->
  let res = State.get_A st lxor State.get_HLp st in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  2

let iXOR_An8 n = fun st ->
  let res = State.get_A st lxor n in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  2

(*(* 16-bit Arithmetic Instructions *)*)
(*let iADD_HLr16 = fun r -> Binary ("ADD", str_HL, str_of_r16 r, 1)*)
(*let iDEC_r16   = fun r -> Unary  ("DEC", str_of_r16 r, 1)*)
(*let iINC_r16   = fun r -> Unary  ("INC", str_of_r16 r, 1)*)
(* *)
(*(* Bit Operations Instructions *)*)
(*let iBIT_u3r8  = fun u r -> Binary ("BIT", str_of_int u, str_of_r8 r, 2)*)
(*let iBIT_u3HLp = fun u ->   Binary ("BIT", str_of_int u, strptr_of_r16 HL, 2)*)
(*let iRES_u3r8  = fun u r -> Binary ("RES", str_of_int u, str_of_r8 r, 2)*)
(*let iRES_u3HLp = fun u ->   Binary ("RES", str_of_int u, strptr_of_r16 HL, 2)*)
(*let iSET_u3r8  = fun u r -> Binary ("SET", str_of_int u, str_of_r8 r, 2)*)
(*let iSET_u3HLp = fun u ->   Binary ("SET", str_of_int u, strptr_of_r16 HL, 2)*)
(*let iSWAP_r8   = fun r ->   Unary  ("SWAP", str_of_r8 r, 2)*)
(*let iSWAP_HLp  =            Unary  ("SWAP", strptr_of_r16 HL, 2)*)
(* *)
(*(* Bit Shift Instructions *)*)
(*let iRL_r8   = fun r -> Unary   ("RL", str_of_r8 r, 2)*)
(*let iRL_HLp  =          Unary   ("RL", strptr_of_r16 HL, 2)*)
(*let iRLA     =          Nullary ("RLA", 1)*)
(*let iRLC_r8  = fun r -> Unary   ("RLC", str_of_r8 r, 2)*)
(*let iRLC_HLp =          Unary   ("RLC", strptr_of_r16 HL, 2)*)
(*let iRLCA    =          Nullary ("RLCA", 1)*)
(*let iRR_r8   = fun r -> Unary   ("RR", str_of_r8 r, 2)*)
(*let iRR_HLp  =          Unary   ("RR", strptr_of_r16 HL, 2)*)
(*let iRRA     =          Nullary ("RRA", 1)*)
(*let iRRC_r8  = fun r -> Unary   ("RRC", str_of_r8 r, 2)*)
(*let iRRC_HLp =          Unary   ("RRC", strptr_of_r16 HL, 2)*)
(*let iRRCA    =          Nullary ("RRCA", 1)*)
(*let iSLA_r8  = fun r -> Unary   ("SLA", str_of_r8 r, 2)*)
(*let iSLA_HLp =          Unary   ("SLA", strptr_of_r16 HL, 2)*)
(*let iSRA_r8  = fun r -> Unary   ("SRA", str_of_r8 r, 2)*)
(*let iSRA_HLp =          Unary   ("SRA", strptr_of_r16 HL, 2)*)
(*let iSRL_r8  = fun r -> Unary   ("SRL", str_of_r8 r, 2)*)
(*let iSRL_HLp =          Unary   ("SRL", strptr_of_r16 HL, 2)*)
(* *)
(*(* Load Instructions *)*)
(*let iLD_rr8    = fun ra rb -> Binary ("LD", str_of_r8 ra, str_of_r8 rb, 1)*)
(*let iLD_rn8    = fun r n ->   Binary ("LD", str_of_r8 r, str_of_int n, 2)*)
(*let iLD_rn16   = fun r n ->   Binary ("LD", str_of_r16 r, str_of_int n, 3)*)
(*let iLD_HLpr8  = fun r ->     Binary ("LD", strptr_of_r16 HL, str_of_r8 r, 1)*)
(*let iLD_HLpn8  = fun n ->     Binary ("LD", strptr_of_r16 HL, str_of_int n, 2)*)
(*let iLD_r8HLp  = fun r ->     Binary ("LD", str_of_r8 r, strptr_of_r16 HL, 1)*)
(*let iLD_r16pA  = fun r ->     Binary ("LD", strptr_of_r16 r, str_A, 1)*)
(*let iLD_n16pA  = fun n ->     Binary ("LD", strptr_of_int n, str_A, 3)*)
(*let iLDH_n16pA = fun n ->     Binary ("LDH", strptr_of_int n, str_A, 2)*)
(*let iLDH_CpA   =              Binary ("LDH", strptr_of_r8 C, str_A, 1)*)
(*let iLD_Ar16p  = fun r ->     Binary ("LD", str_A, strptr_of_r16 r, 1)*)
(*let iLD_An16p  = fun n ->     Binary ("LD", str_A, strptr_of_int n, 3)*)
(*let iLDH_An16p = fun n ->     Binary ("LDH", str_A, strptr_of_int n, 2)*)
(*let iLDH_ACp   =              Binary ("LDH", str_A, strptr_of_r8 C, 1)*)
(*let iLD_HLIpA  =              Binary ("LD", strptrinc_of_r16 HL, str_A, 1)*)
(*let iLD_HLDpA  =              Binary ("LD", strptrdec_of_r16 HL, str_A, 1)*)
(*let iLD_AHLIp  =              Binary ("LD", str_A, strptrinc_of_r16 HL, 1)*)
(*let iLD_AHLDp  =              Binary ("LD", str_A, strptrdec_of_r16 HL, 1)*)
(* *)
(*(* Jumps and Subroutines *)*)
(*let iCALL_n16  = fun n ->   Unary ("CALL", str_of_int n, 3)*)
(*let iCALL_cn16 = fun c n -> Binary ("CALL", str_of_cond c, str_of_int n, 3)*)
(*let iJP_HL     =            Unary ("JP", str_HL, 1)*)
(*let iJP_n16    = fun n ->   Unary ("JP", str_of_int n, 3)*)
(*let iJP_cn16   = fun c n -> Binary ("JP", str_of_cond c, str_of_int n, 3)*)
(*let iJR_n8    = fun n ->    Unary ("JR", str_of_int n, 2)*)
(*let iJR_cn8   = fun c n ->  Binary ("JR", str_of_cond c, str_of_int n, 2)*)
(*let iRET_c     = fun c ->   Unary ("RET", str_of_cond c, 1)*)
(*let iRET       =            Nullary ("RET", 1)*)
(*let iRETI      =            Nullary ("RETI", 1)*)
(*let iRST_vec   = fun n ->   Unary ("RST", str_of_int n, 1)*)
(* *)
(*(* Stack Operations Instructions *)*)
(*let iADD_HLSP  =          Binary ("ADD", str_HL, str_SP, 1)*)
(*let iADD_SPe8  = fun e -> Binary ("ADD", str_SP, str_of_int e, 2)*)
(*let iDEC_SP    =          Unary  ("DEC", str_SP, 1)*)
(*let iINC_SP    =          Unary  ("INC", str_SP, 1)*)
(*let iLD_SPn16  = fun n -> Binary ("LD", str_SP, str_of_int n, 3)*)
(*let iLD_n16pSP = fun n -> Binary ("LD", strptr_of_int n, str_SP, 3)*)
(*let iLD_HLSPe8 = fun e -> Binary ("LD", str_HL, str_SP ^ "+" ^ str_of_int e, 2)*)
(*let iLD_SPHL   =          Binary ("LD", str_SP, str_HL, 1)*)
(*let iPOP_AF    =          Unary  ("POP", str_AF, 1)*)
(*let iPOP_r16   = fun r -> Unary  ("POP", str_of_r16 r, 1)*)
(*let iPUSH_AF   =          Unary  ("PUSH", str_AF, 1)*)
(*let iPUSH_r16  = fun r -> Unary  ("PUSH", str_of_r16 r, 1)*)
(* *)
(*(* Miscellaneous Instructions *)*)
(*let iCCF  = Nullary ("CCF", 1)*)
(*let iCPL  = Nullary ("CPL", 1)*)
(*let iDAA  = Nullary ("DAA", 1)*)
(*let iDI   = Nullary ("DI", 1)*)
(*let iEI   = Nullary ("EI", 1)*)
(*let iHALT = Nullary ("HALT", 1)*)
(*let iNOP  = Nullary ("NOP", 1)*)
(*let iSCF  = Nullary ("SCF", 1)*)
(*let iSTOP = fun n -> Unary ("STOP", str_of_int n, 2)*)

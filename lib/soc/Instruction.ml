(* Instruction type *)

type next_action = Next | Jump | Halt | Stop

type instruction = State.t -> State.t * next_action * int

type condition = Cnz | Cz | Cnc | Cc

(* 8-bit arithmetic and logic instructions *)
let iADC_Ar8 r8 : instruction = fun st ->
  let a, r = st.regs._A, State.get_r8 st r8 in
  let c = State.get_flag st Regs.Flag_c in
  let sum = a + r + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + r land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  Next, 1

let iADC_AHLp : instruction = fun st ->
  let a = st.regs._A in
  let hlp = State.get_HLp st in
  let c = State.get_flag st Regs.Flag_c in
  let sum = a + hlp + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + hlp land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  Next, 2

let iADC_An8 n : instruction = fun st ->
  let a, c = st.regs._A, State.get_flag st (Regs.Flag_c) in
  let sum = a + n + c in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + n land 0xF + c > 0xF) ~c:(sum > 0xFF) (),
  Next, 2

let iADD_Ar8 r8 : instruction = fun st ->
  let a, r = st.regs._A, State.get_r8 st r8 in
  let sum = a + r in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + r land 0xF > 0xF) ~c:(sum > 0xFF) (),
  Next, 1

let iADD_AHLp : instruction = fun st ->
  let a = st.regs._A in
  let hlp = State.get_HLp st in
  let sum = a + hlp in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + hlp land 0xF > 0xF) ~c:(sum > 0xFF) (),
  Next, 2

let iADD_An8 n : instruction = fun st ->
  let a = st.regs._A in
  let sum = a + n in let res = Intops.u8 sum in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:(a land 0xF + n land 0xF > 0xF) ~c:(sum > 0xFF) (),
  Next, 2

let iAND_Ar8 r8 : instruction = fun st ->
  let result = st.regs._A land State.get_r8 st r8 in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true  ~c:false (),
  Next, 1

let iAND_AHLp : instruction = fun st ->
  let result = st.regs._A land State.get_HLp st in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true ~c:false (),
  Next, 2

let iAND_An8 n : instruction = fun st ->
  let result = st.regs._A land n in
  State.set_flags (State.set_A st result) ~z:(result = 0) ~n:false
  ~h:true ~c:false (),
  Next, 2

let iCP_Ar8 r : instruction = fun st ->
  let a, y = st.regs._A, State.get_r8 st r in
  let result = a - y in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < y land 0xF) ~c:(y > a) (),
  Next, 1

let iCP_AHLp : instruction = fun st ->
  let a, hlp = st.regs._A, State.get_HLp st in
  let result = a - hlp in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF) ~c:(hlp > a) (),
  Next, 2

let iCP_An8 n : instruction = fun st ->
  let a = st.regs._A in
  let result = a - n in
  State.set_flags st ~z:(result = 0) ~n:true
  ~h:(a land 0xF < n land 0xF) ~c:(n > a) (),
  Next, 2

let iDEC_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let dec = x - 1 in let res = if dec < 0 then 0xFF else dec in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:true
  ~h:(x land 0x0F = 0x0) (),
  Next, 1

let iDEC_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let dec = hlp - 1 in let res = if dec < 0 then 0xFF else dec in
  State.set_flags (State.Bus.set8 st (State.get_HL st) res) ~z:(res = 0)
  ~n:true ~h:(hlp land 0x0F = 0x0) (),
  Next, 3

let iINC_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let inc = x + 1 in let res = if inc > 0xFF then 0 else inc in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:(x land 0x0F = 0x0F) (),
  Next, 1

let iINC_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let inc = hlp + 1 in let res = if inc > 0xFF then 0 else inc in
  State.set_flags (State.Bus.set8 st (State.get_HL st) res) ~z:(res = 0)
  ~n:false ~h:(hlp land 0x0F = 0x0F) (),
  Next, 3

let iOR_Ar8 r : instruction = fun st ->
  let res = st.regs._A lor State.get_r8 st r in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 1

let iOR_AHLp : instruction = fun st ->
  let res = st.regs._A lor State.get_HLp st in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 2

let iOR_An8 n : instruction = fun st ->
  let res = st.regs._A lor n in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 2

let iSBC_Ar8 r : instruction = fun st ->
  let a, x, c = st.regs._A, State.get_r8 st r, State.get_flag st Flag_c in
  let sub = a - x - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < x land 0xF + c) ~c:(sub < 0) (),
  Next, 1

let iSBC_AHLp : instruction = fun st ->
  let a, hlp, c = st.regs._A, State.get_HLp st, State.get_flag st Flag_c in
  let sub = a - hlp - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF + c) ~c:(sub < 0) (),
  Next, 2

let iSBC_An8 n : instruction = fun st ->
  let a, c = st.regs._A, State.get_flag st Flag_c in
  let sub = a - n - c in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < n land 0xF + c) ~c:(sub < 0) (),
  Next, 2

let iSUB_Ar8 r : instruction = fun st ->
  let a, x = st.regs._A, State.get_r8 st r in
  let sub = a - x in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < x land 0xF) ~c:(sub < 0) (),
  Next, 1

let iSUB_AHLp : instruction = fun st ->
  let a, hlp = st.regs._A, State.get_HLp st in
  let sub = a - hlp in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < hlp land 0xF) ~c:(sub < 0) (),
  Next, 2

let iSUB_An8 n : instruction = fun st ->
  let a = st.regs._A in
  let sub = a - n in let res = if sub < 0 then 0x100 + sub else sub in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:true
  ~h:(a land 0xF < n land 0xF) ~c:(sub < 0) () ,
  Next, 2

let iXOR_Ar8 r : instruction = fun st ->
  let res = st.regs._A lxor State.get_r8 st r in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 1

let iXOR_AHLp : instruction = fun st ->
  let res = st.regs._A lxor State.get_HLp st in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 2

let iXOR_An8 n : instruction = fun st ->
  let res = st.regs._A lxor n in
  State.set_flags (State.set_A st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 2

(* 16-bit arithmetic instructions *)
let iADD_HLr16 r : instruction = fun st ->
  let hl, x = State.get_HL st, State.get_r16 st r in
  let sum = hl + x in let res = Intops.u16 sum in
  State.set_flags (State.set_HL st res) ~n:false
  ~h:(hl land 0xFFF + x land 0xFFF > 0xFFF) ~c:(sum > 0xFFFF) (),
  Next, 2

let iDEC_r16 r : instruction = fun st ->
  let x = State.get_r16 st r in
  let dec = x - 1 in let res = if dec < 0 then 0xFFFF else dec in
  State.set_r16 st r res,
  Next, 2

let iINC_r16 r : instruction = fun st ->
  let x = State.get_r16 st r in
  let inc = x + 1 in let res = if inc > 0xFFFF then 0 else inc in
  State.set_r16 st r res,
  Next, 2

(* Bit Operations Instructions *)
let iBIT_u3r8 u r : instruction = fun st ->
  let x = State.get_r8 st r in
  State.set_flags st ~z:(x land (1 lsl u) = 0) ~n:false ~h:true (),
  Next, 2

let iBIT_u3HLp u : instruction = fun st ->
  let hlp = State.get_HLp st in
  State.set_flags st ~z:(hlp land (1 lsl u) = 0) ~n:false ~h:true (),
  Next, 3

let iRES_u3r8 u r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = x land (lnot (1 lsl u)) in
  State.set_r8 st r res,
  Next, 2

let iRES_u3HLp u : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = hlp land (lnot (1 lsl u)) in
  State.set_HLp st res,
  Next, 4

let iSET_u3r8 u r : instruction = fun st ->
  let x = State.get_r8 st r in
  State.set_r8 st r (x lor (1 lsl u)),
  Next, 2

let iSET_u3HLp u : instruction = fun st ->
  let hlp = State.get_HLp st in
  State.set_HLp st (hlp lor (1 lsl u)),
  Next, 4

let iSWAP_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = ((x land 0xF lsl 4) lor (x land 0xF0 lsr 4)) in
  State.set_flags (State.set_r8 st r res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 2
let iSWAP_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = ((hlp land 0xF lsl 4) lor (hlp land 0xF lsr 4)) in
  State.set_flags (State.set_HLp st res)
  ~z:(res = 0) ~n:false ~h:false ~c:false (),
  Next, 4

(* Bit Shift Instructions *)
let iRL_r8 r : instruction = fun st ->
  let x, c = State.get_r8 st r, State.get_flag st Flag_c in
  let shifted = x lsl 1 lor c in let res = Intops.u8 shifted in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0x80 > 0) (),
  Next, 2

let iRL_HLp : instruction = fun st ->
  let hlp, c = State.get_HLp st, State.get_flag st Flag_c in
  let shifted = hlp lsl 1 lor c in let res = Intops.u8 shifted in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0x80 > 0) (),
  Next, 4

let iRLA : instruction = fun st ->
  let a, c = st.regs._A, State.get_flag st Flag_c in
  let shifted = a lsl 1 lor c in let res = Intops.u8 shifted in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(a land 0x80 > 0) (),
  Next, 1

let iRLC_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let shifted = x lsl 1 in
  let res = shifted land 0x100 lsr 8 lor shifted |> Intops.u8 in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0x80 > 0) (),
  Next, 2

let iRLC_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let shifted = hlp lsl 1 in
  let res = shifted land 0x100 lsr 8 lor shifted |> Intops.u8 in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0x80 > 0) (),
  Next, 4

let iRLCA : instruction = fun st ->
  let a = st.regs._A in
  let shifted = a lsl 1 in
  let res = shifted land 0x100 lsr 8 lor shifted |> Intops.u8 in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(a land 0x80 > 0) (),
  Next, 1

let iRR_r8 r : instruction = fun st ->
  let x, c = State.get_r8 st r, State.get_flag st Flag_c in
  let shifted = x lsr 1 lor (c lsl 7) in let res = Intops.u8 shifted in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0b1 > 0) (),
  Next, 2

let iRR_HLp : instruction = fun st ->
  let hlp, c = State.get_HLp st, State.get_flag st Flag_c in
  let shifted = hlp lsr 1 lor (c lsl 7) in let res = Intops.u8 shifted in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0b1 > 0) (),
  Next, 4

let iRRA : instruction = fun st ->
  let a, c = st.regs._A, State.get_flag st Flag_c in
  let shifted = a lsr 1 lor (c lsl 7) in let res = Intops.u8 shifted in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(a land 0b1 > 0) (),
  Next, 1

let iRRC_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = x lsr 1 lor (x land 0b1 lsl 7) |> Intops.u8 in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0b1 > 0) (),
  Next, 2

let iRRC_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = hlp lsr 1 lor (hlp land 0b1 lsl 7) |> Intops.u8 in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0b1 > 0) (),
  Next, 4

let iRRCA : instruction = fun st ->
  let a = st.regs._A in
  let res = a lsr 1 lor (a land 0b1 lsl 7) |> Intops.u8 in
  State.set_flags (State.set_A st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(a land 0b1 > 0) (),
  Next, 1

let iSLA_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = x lsl 1 |> Intops.u8 in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0x80 > 0) (),
  Next, 2

let iSLA_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = hlp lsl 1 |> Intops.u8 in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0x80 > 0) (),
  Next, 4

let iSRA_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = x lsr 1 lor (x land 0x80)|> Intops.u8 in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0b1 > 0) (),
  Next, 2

let iSRA_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = hlp lsr 1 lor (hlp land 0x80) |> Intops.u8 in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0b1 > 0) (),
  Next, 4

let iSRL_r8 r : instruction = fun st ->
  let x = State.get_r8 st r in
  let res = x lsr 1 |> Intops.u8 in
  State.set_flags (State.set_r8 st r res) ~z:(res = 0) ~n:false
  ~h:false ~c:(x land 0b1 > 0) (),
  Next, 2

let iSRL_HLp : instruction = fun st ->
  let hlp = State.get_HLp st in
  let res = hlp lsr 1 |> Intops.u8 in
  State.set_flags (State.set_HLp st res) ~z:(res = 0) ~n:false
  ~h:false ~c:(hlp land 0b1 > 0) (),
  Next, 4

(* Load Instructions *)
let iLD_rr8 rl rr : instruction = fun st ->
  State.set_r8 st rl (State.get_r8 st rr),
  Next, 1

let iLD_rn8 rl n : instruction = fun st ->
  State.set_r8 st rl n,
  Next, 2

let iLD_rn16 rl n : instruction = fun st ->
  State.set_r16 st rl n,
  Next, 3

let iLD_HLpr8 r : instruction = fun st ->
  State.set_HLp st (State.get_r8 st r),
  Next, 2

let iLD_HLpn8 n : instruction = fun st ->
  State.set_HLp st n,
  Next, 3

let iLD_r8HLp r : instruction = fun st ->
  State.set_r8 st r (State.get_HLp st),
  Next, 2

let iLD_r16pA r : instruction = fun st ->
  State.Bus.set8 st (State.get_r16 st r) (st.regs._A),
  Next, 2

let iLD_n16pA n : instruction = fun st ->
  State.Bus.set8 st n (st.regs._A),
  Next, 4

let iLDH_n16pA n : instruction = fun st ->
  State.Bus.set8 st (0xFF00 + n) (st.regs._A),
  Next, 3

let iLDH_CpA : instruction = fun st ->
  State.Bus.set8 st (st.regs._C + 0xFF00) (st.regs._A),
  Next, 2

let iLD_Ar16p r : instruction = fun st ->
  State.set_A st (State.Bus.get8 st (State.get_r16 st r)),
  Next, 2

let iLD_An16p n : instruction = fun st ->
  State.set_A st (State.Bus.get8 st n),
  Next, 4

let iLDH_An16p n : instruction = fun st ->
  State.set_A st (State.Bus.get8 st (0xFF00 + n)),
  Next, 3

let iLDH_ACp : instruction = fun st ->
  State.set_A st (State.Bus.get8 st (st.regs._C + 0xFF00)),
  Next, 2

let iLD_HLIpA : instruction = fun st ->
  let st' = State.set_HLp st (st.regs._A) in
  State.set_HL st' (State.get_HL st' + 1 |> Intops.u16),
  Next, 2

let iLD_HLDpA : instruction = fun st ->
  let st' = State.set_HLp st (st.regs._A) in
  let res = State.get_HL st' - 1 in
  State.set_HL st' (if res < 0 then 0xFFFF else res),
  Next, 2

let iLD_AHLIp : instruction = fun st ->
  let st' = State.set_A st (State.get_HLp st) in
  State.set_HL st' (State.get_HL st' +1 |> Intops.u16),
  Next, 2

let iLD_AHLDp : instruction = fun st ->
  let st' = State.set_A st (State.get_HLp st) in
  let res = State.get_HL st' - 1 in
  State.set_HL st' (if res < 0 then 0xFFFF else res),
  Next, 2

(* Jumps and Subroutines *)

let check_condition st =
  function
  | Cnz    -> State.get_flag st Flag_z == 0
  | Cz     -> State.get_flag st Flag_z == 1
  | Cnc    -> State.get_flag st Flag_c == 0
  | Cc     -> State.get_flag st Flag_c == 1

let iCALL_n16 n : instruction = fun st ->
  State.set_PC (State.set_SPp (State.dec_SP st) (State.get_PC st)) n,
  Jump, 6

let iCALL_cn16 c n : instruction = fun st ->
  if check_condition st c
  then iCALL_n16 n st
  else st, Next, 3

let iJP_HL : instruction = fun st ->
  State.set_PC st (State.get_HL st),
  Jump, 1

let iJP_n16 n : instruction = fun st ->
  State.set_PC st n,
  Jump, 4

let iJP_cn16 c n : instruction = fun st ->
  if check_condition st c
  then iJP_n16 n st
  else st, Jump, 3

let iJR_n8 n : instruction = fun st ->
  State.adv_PC st (Intops.s8 n),
  Jump, 3

let iJR_cn8 c n : instruction = fun st ->
  if check_condition st c
  then iJR_n8 n st
  else st, Next, 2

let iRET : instruction = fun st ->
  State.set_PC st (State.get_SPp st), Jump, 4

let iRET_c c : instruction = fun st ->
  if check_condition st c
  then let st',act,_ = iRET st in st', act, 5
  else st, Next, 2

let iRETI : instruction = fun st ->
  let st',act,_ = iRET { st with ime = Enabled } in
  st', act, 4

let iRST_vec n : instruction = fun st ->
  let st',act,_ = iCALL_n16 n st in
  st', act, 4

(* Stack Operations Instructions *)
let iADD_HLSP : instruction = fun st ->
  let hl, sp = State.get_HL st, State.get_SP st in
  let sum = hl + sp in let res = Intops.u16 sum in
  State.set_flags (State.set_HL st res) ~n:false
  ~h:(hl land 0xFFF + sp land 0xFFF > 0xFFF) ~c:(sum > 0xFFFF) (),
  Next, 2

let iADD_SPe8 e : instruction = fun st ->
  let sp = State.get_SP st in
  let sum = sp + (Intops.s8 e) in let res = Intops.u16 sum in
  State.set_flags (State.set_SP st res) ~z:false ~n:false
  ~h:(sp land 0xF + e land 0xF > 0xF) ~c:(sp land 0xFF + e land 0xFF > 0xFF) (),
  Next, 4

let iDEC_SP : instruction = fun st ->
  State.dec_SP st,
  Next, 2

let iINC_SP : instruction = fun st ->
  State.inc_SP st,
  Next, 2

let iLD_SPn16 n : instruction = fun st ->
  State.set_SP st n,
  Next, 3

let iLD_n16pSP n : instruction = fun st ->
  State.Bus.set16 st n (State.get_SP st),
  Next, 5

let iLD_HLSPe8 e : instruction = fun st ->
  let sp = State.get_SP st in
  let sum = sp + (Intops.s8 e) in let res = Intops.u16 sum in
  State.set_flags (State.set_HL st res) ~z:false ~n:false
  ~h:(sp land 0xF + e land 0xF > 0xF) ~c:(sp land 0xFF + e land 0xFF > 0xFF) (),
  Next, 3

let iLD_SPHL : instruction = fun st ->
  State.set_SP st (State.get_HL st),
  Next, 2

let iPOP_AF : instruction = fun st ->
  let sp = State.get_SPp st in
  State.set_flags (State.set_r16 (State.inc_SP st) AF sp) ~z:(sp land 0x80 > 0)
  ~n:(sp land 0x40 > 0) ~h:(sp land 0x20 > 0) ~c:(sp land 0x10 > 0) (),
  Next, 3

let iPOP_r16 r : instruction = fun st ->
  let sp = State.get_SPp st in
  State.set_flags (State.set_r16 (State.inc_SP st) r sp) ~z:(sp land 0x80 > 0)
  ~n:(sp land 0x40 > 0) ~h:(sp land 0x20 > 0) ~c:(sp land 0x10 > 0) (),
  Next, 3

let iPUSH_AF : instruction = fun st ->
  State.dec_SP (State.set_SPp st (State.get_r16 st AF)),
  Next, 4

let iPUSH_r16 r : instruction = fun st ->
  State.dec_SP (State.set_SPp st (State.get_r16 st r)),
  Next, 4

(* Miscellaneous Instructions *)
let iCCF : instruction = fun st ->
  State.set_flags st ~n:false ~h:false ~c:(State.get_flag st Flag_c == 0) (),
  Next, 1

let iCPL : instruction = fun st ->
  State.set_A st (st.regs._A |> lnot),
  Next, 1

(* May be wrong *)
let iDAA : instruction = fun st ->
  let n, h = State.get_flag st Flag_n, State.get_flag st Flag_h in
  let a, c = st.regs._A, State.get_flag st Flag_c in
  let a' =
    match n with
    | 0 ->
      let x =
        if c == 1 || a > 0x99 then a + 0x60 else a in
        if h == 1 || (x land 0x0F) > 0x09 then x + 0x06 else x
    | 1 ->
      let x =
        if c == 1 then a - 0x60 else a in
        if h == 1 then x - 0x06 else x
  in
  State.set_flags (State.set_A st a') ~z:(a = 0) ~h:false
  ~c:(c == 1 || a > 0x99) (),
  Next, 1

let iDI : instruction = fun st ->
  { st with ime = Disabled }, Next, 1

let iEI : instruction = fun st ->
  {st with ime = Enabling }, Next, 1

let iHALT : instruction = fun st ->
  match st.ime with
  | Enabled  -> { st with activity = Halted }, Halt, 1
  | Disabled ->
    let _if, _ie = State.Bus.get8 st 0xFF0F, State.Bus.get8 st 0xFFFF in
    if _if land _ie = 0
    then { st with activity = Halted }, Halt, 1
    (* HALT BUG TODO *)
    else st, Next, 1

let iNOP : instruction = fun st ->
  st, Next, 1

let iSCF : instruction = fun st ->
  State.set_flags st ~n:false ~h:false ~c:true (), Next, 1

let iSTOP _ : instruction = fun st ->
  { st with activity = Stopped 0; timer = IOregs.Timer.switch_speed (IOregs.Timer.reset_div st.timer) }, Stop, 0


(* Not an actual instruction *)
let interrupt_service_routine handler : instruction = fun st ->
  State.set_PC (State.set_SPp (State.dec_SP st) (State.get_PC st)) handler,
  Jump, 5

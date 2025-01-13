open Intops

type r8  = A | B | C | D | E | H | L
type r16 = AF | BC | DE | HL | SP | PC
type flag = Flag_z | Flag_n | Flag_h | Flag_c

type regfile = {
  _A : int (* u8 *); _B : int (* u8 *); _C : int (* u8 *); _D : int (* u8 *);
  _E : int (* u8 *); _H : int (* u8 *); _L : int (* u8 *);
  _AF : int (* u16 *); _BC : int (* u16 *); _DE : int (* u16 *);
   _HL: int (* u16 *); _SP : int (* u16 *); _PC : int (* u16 *)
  }

type flags = {
  z : bool (* bit *); n : bool (* bit *); h : bool (* bit *); c : bool (* bit *)
}

let initial_flags = {
  z = true; n = false; h = false; c = false
}

let initial_regfile = {
  _A = 0; _B = 0; _C = 0; _D = 0;
  _E = 0; _H = 0; _L = 0;
  _AF = 0; _BC = 0; _DE = 0; _HL= 0;
  _SP = 0; _PC = 0
}




let set_r8 rf r v =
  match r with
  | A -> { rf with _A = v; _AF = set_high_byte rf._AF v }
  | B -> { rf with _B = v; _BC = set_high_byte rf._BC v }
  | C -> { rf with _C = v; _BC = set_low_byte rf._BC v }
  | D -> { rf with _D = v; _DE = set_high_byte rf._DE v }
  | E -> { rf with _E = v; _DE = set_low_byte rf._DE v }
  | H -> { rf with _H = v; _HL = set_high_byte rf._HL v }
  | L -> { rf with _L = v; _HL = set_low_byte rf._HL v }

let set_r16 rf rr v =
  match rr with
  | AF -> { rf with _AF = v; _A = get_high v }
  | BC -> { rf with _BC = v; _B = get_high v; _C = get_low v }
  | DE -> { rf with _DE = v; _D = get_high v; _E = get_low v }
  | HL -> { rf with _HL = v; _H = get_high v; _L = get_low v }
  | SP -> { rf with _SP = v }
  | PC -> { rf with _PC = v }

let get_r8 rf r =
  match r with
  | A -> rf._A
  | B -> rf._B
  | C -> rf._C
  | D -> rf._D
  | E -> rf._E
  | H -> rf._H
  | L -> rf._L

let get_r16 rf rr =
  match rr with
  | AF -> rf._AF
  | BC -> rf._BC
  | DE -> rf._DE
  | HL -> rf._HL
  | SP -> rf._SP
  | PC -> rf._PC

let get_flag fs f =
  match f with
  | Flag_z -> if fs.z then 1 else 0
  | Flag_n -> if fs.n then 1 else 0
  | Flag_h -> if fs.h then 1 else 0
  | Flag_c -> if fs.h then 1 else 0

let set_flag fs f v =
  match f with
  | Flag_z -> { fs with z = v }
  | Flag_n -> { fs with n = v }
  | Flag_h -> { fs with h = v }
  | Flag_c -> { fs with c = v }

let initial_regfile_cgb =
  let aux8 r v rf = set_r8 rf r v in
  let aux16 r v rf = set_r16 rf r v in
  initial_regfile |> (aux8 A 0x11) |> (aux8 D 0xFF) |> (aux8 E 0x56) |>
  (aux8 L 0x0D) |> (aux16 PC 0x100) |> (aux16 SP 0xFFFE)

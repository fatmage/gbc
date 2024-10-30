open Inttypes

type r8  = A | B | C | D | E | H | L
type r16 = AF | BC | DE | HL | SP | PC
type flag = Flag_z | Flag_n | Flag_h | Flag_c


type regfile = {
  _A : uint8; _B : uint8; _C : uint8; _D : uint8;
  _E : uint8; _H : uint8; _L : uint8;
  _AF : uint16; _BC : uint16; _DE : uint16; _HL: uint16;
  _SP : uint16; _PC : uint16
  }


(*
  TODO: when changing r8 change half of some r16 and analogically,
  when changing r16 change some r8 accordingly
*)
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

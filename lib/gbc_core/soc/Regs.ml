open Utils

type r8  = A | B | C | D | E | H | L
type r16 = AF | BC | DE | HL | SP | PC
type flag = Flag_z | Flag_n | Flag_h | Flag_c

type flags = {
  z : bool (* bit *); n : bool (* bit *); h : bool (* bit *); c : bool; (* bit *) low_nibble : int
}

type regfile = {
  _A : int (* u8 *); _B : int (* u8 *); _C : int (* u8 *); _D : int (* u8 *);
  _E : int (* u8 *); _H : int (* u8 *); _L : int (* u8 *);
  _AF : int (* u16 *); _BC : int (* u16 *); _DE : int (* u16 *);
   _HL: int (* u16 *); _SP : int (* u16 *); _PC : int (* u16 *);
   flags : flags
  }



let initial_flags = {
  z = true; n = false; h = false; c = false; low_nibble = 0
}

let initial_regfile = {
  _A = 0; _B = 0; _C = 0; _D = 0;
  _E = 0; _H = 0; _L = 0;
  _AF = 0; _BC = 0; _DE = 0; _HL= 0;
  _SP = 0; _PC = 0;
  flags = initial_flags
}

let flags_to_int {z;n;h;c;low_nibble} =
  let seventh = if z then 0b10000000 else 0 in
  let sixth   = if n then 0b01000000 else 0 in
  let fifth   = if h then 0b00100000 else 0 in
  let fourth  = if c then 0b00010000 else 0 in
  seventh lor sixth lor fifth lor fourth lor low_nibble

let int_to_flags v =
  let z = v land 0b10000000 > 0 in
  let n = v land 0b01000000 > 0 in
  let h = v land 0b00100000 > 0 in
  let c = v land 0b00010000 > 0 in
  let low_nibble = v land 0x0F in
  { z; n; h; c; low_nibble }


let set_r8 rf r v =
  match r with
  | A -> { rf with _A = v }
  | B -> { rf with _B = v; _BC = set_high_byte rf._BC v }
  | C -> { rf with _C = v; _BC = set_low_byte rf._BC v }
  | D -> { rf with _D = v; _DE = set_high_byte rf._DE v }
  | E -> { rf with _E = v; _DE = set_low_byte rf._DE v }
  | H -> { rf with _H = v; _HL = set_high_byte rf._HL v }
  | L -> { rf with _L = v; _HL = set_low_byte rf._HL v }

let set_r16 rf rr v =
  match rr with
  | AF -> { rf with _A = get_high v; flags = int_to_flags (get_low v) }
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
  | AF -> (rf._A lsl 8) lor (flags_to_int rf.flags)
  | BC -> rf._BC
  | DE -> rf._DE
  | HL -> rf._HL
  | SP -> rf._SP
  | PC -> rf._PC

let get_flag rs f =
  match f with
  | Flag_z -> if rs.flags.z then 1 else 0
  | Flag_n -> if rs.flags.n then 1 else 0
  | Flag_h -> if rs.flags.h then 1 else 0
  | Flag_c -> if rs.flags.c then 1 else 0

let set_flag rs f v =
  match f with
  | Flag_z -> { rs with flags = { rs.flags with z = v } }
  | Flag_n -> { rs with flags = { rs.flags with n = v } }
  | Flag_h -> { rs with flags = { rs.flags with h = v } }
  | Flag_c -> { rs with flags = { rs.flags with c = v } }

let initial_regfile_cgb =
  let aux8 r v rf = set_r8 rf r v in
  let aux16 r v rf = set_r16 rf r v in
  initial_regfile |> (aux8 A 0x11) |> (aux8 D 0xFF) |> (aux8 E 0x56) |>
  (aux8 L 0x0D) |> (aux16 PC 0x100) |> (aux16 SP 0xFFFE)


let print_flags {z;n;h;c;low_nibble} =
  print_endline @@
    "z:" ^ (string_of_bool z) ^ " n:" ^ (string_of_bool n) ^
    " h:" ^ (string_of_bool h) ^ " c:" ^ (string_of_bool c) ^
    " low: " ^ (Printf.sprintf "0x%02X" low_nibble)


let print_registers regs =
  Utils.print_hex "AF" @@ get_r16 regs AF;
  Utils.print_hex "BC" @@ get_r16 regs BC;
  Utils.print_hex "DE" @@ get_r16 regs DE;
  Utils.print_hex "HL" @@ get_r16 regs HL;
  Utils.print_hex "SP" @@ get_r16 regs SP;
  Utils.print_hex "PC" @@ get_r16 regs PC

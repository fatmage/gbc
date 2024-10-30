module type IntType = sig
  type t

  val max_val : t
  val zero : t
  val one : t
  val two : t

  val compare : t -> t -> int
  val eq : t -> t -> bool
  val neq : t -> t -> bool
  val gt : t -> t -> bool
  val gte : t -> t -> bool
  val lt : t -> t -> bool
  val lte : t -> t -> bool
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shr : t -> t -> t
  val shl : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t

  val of_int : int -> t
  val to_int : t -> int

  val to_string : t -> string
  val to_hexstring : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_hex : Format.formatter -> t -> unit
end


let make_int_module : int -> (module IntType) = fun max_val -> (module struct
  type t = int

  let max_val = max_val
  let zero = 0
  let one = 1
  let two = 2

  let compare = compare
  let eq a b = a == b
  let neq a b = a != b
  let gt a b = a > b
  let lt a b = a < b
  let gte a b = (eq a b) || (gt a b)
  let lte a b = (eq a b) || (lt a b)
  let logand a b = a land b
  let logor a b = a lor b
  let logxor a b = a lxor b
  let shr a b = Int.shift_right a b
  let shl a b = (Int.shift_left a b) land max_val
  let add a b = (a + b) land max_val
  let sub a b = let res = a - b in if res < 0 then (max_val + res + 1) else res
  let mul a b = (a * b) land max_val
  let div a b = (a / b) land max_val
  let rem a b = (a mod b) land max_val
  let succ a = add a one
  let pred a = sub a one

  let of_int v = v land max_val
  let to_int v = v

  let to_string = Int.to_string
  let to_hexstring v = Printf.sprintf "0x%x" v
  let pp fmt v = Format.fprintf fmt "%s" (to_string v)
  let pp_hex fmt v = Format.fprintf fmt "%s" (to_hexstring v)

end : IntType
)

module U8 : IntType = struct
  module B = (val make_int_module 0xFF : IntType)
  include B
end


module U16 : IntType = struct
  module B = (val make_int_module 0xFFFF : IntType)
  include B
end

type uint8 = U8.t
type uint16 = U16.t

let u8_to_16 u8 = U16.of_int (U8.to_int u8)
let u16_to_8 u16 = U8.of_int (U16.to_int u16)

let u8_to_char u8 = u8 |> U8.to_int |> char_of_int
let char_to_u8 c = c |> int_of_char |> U8.of_int

let set_low_byte u16 u8 =
  U16.logor (U16.logand u16 (U16.of_int 0xFF)) (u8_to_16 u8)
let set_high_byte u16 u8 =
  U16.of_int
  (((U16.to_int u16) land 0xFF00) lor
   ((U8.to_int u8) lsl 8))

let get_high u16 = U8.of_int ((U16.to_int u16) lsr 8)
let get_low u16 = U8.of_int ((U16.to_int u16) land 0xFF)
